{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Redemption codes: what they look like on the wire, and how a client decides
-- one is real without asking anybody.
--
-- A code authorises registering a name of at least @minLength@ characters for
-- @years@ years, and carries an expiry fixed at issuance. It is an RSA blind
-- signature (RFC 9474), so the issuer never saw the finished token — which is
-- what lets the service record a spent code without learning who was issued it.
-- The token is therefore its own nullifier.
--
-- The client verifies **offline, against a pinned public key**: there is no
-- \"check this code\" RPC, because that would hand the service an oracle for
-- probing codes. The tier is not carried in the code — it comes from whichever
-- pinned key verifies, so it cannot lie.
module Simplex.Chat.Names.Codes
  ( PinnedIssuerKey (..),
    VerifiedCode (..),
    CodeError (..),
    codeErrorText,
    issuerKeys,
    verifyCode,
    encodeCode,
    codePrefix,
#if defined(dev_codes)
    devIssuerKey,
    devIssuerPrivate,
    signDevCode,
#endif
  )
where

import qualified Crypto.Hash.Algorithms as H
import qualified Crypto.PubKey.RSA as RSA
import qualified Crypto.PubKey.RSA.PSS as PSS
import qualified Data.ByteArray.Encoding as BAE
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeLatin1, encodeUtf8)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Word (Word32)

-- | A published verification key. The tier is data on the entry, not a lookup
-- elsewhere, so adding a production key later is a patch to one list.
-- | A published verification key.
--
-- Everything the issuer attests lives here, not in the code: under blind
-- issuance the issuer signs a value it cannot read, so it cannot check the
-- content, and any attribute carried inside the message would be one the
-- /holder/ chose. Tier and expiry are therefore properties of the key, and a
-- key is issued per (tier, expiry cohort).
data PinnedIssuerKey = PinnedIssuerKey
  { pikLabel :: Text,
    pikKey :: RSA.PublicKey,
    pikMinLength :: Int,
    pikYears :: Word32,
    -- | When codes signed by this key stop working. A cohort, never per-holder:
    -- a distinct expiry per buyer would identify them as surely as a serial
    -- number.
    pikExpires :: UTCTime
  }

data VerifiedCode = VerifiedCode
  { vcMinLength :: Int,
    vcYears :: Word32,
    vcExpires :: UTCTime,
    vcLabel :: Text,
    -- | The nullifier: what a service records to stop the code being spent
    -- twice.
    --
    -- Recording it does not make the code linkable to whoever was issued it.
    -- Under RFC 9474 the /holder/ chooses this value and blinds it; the issuer
    -- signs something it cannot read and never learns the nonce, so it has
    -- nothing to match a redemption against.
    --
    -- Deliberately the decoded nonce and not the code string — base64's final
    -- character carries unused bits, so two different strings decode to the same
    -- payload and a ledger keyed on the text could be bypassed by changing one
    -- character. It is also safer than keying on the signature: PSS is
    -- randomised, so one message can yield more than one valid signature.
    vcNonce :: ByteString
  }
  deriving (Eq, Show)

data CodeError
  = CEBadPrefix
  | CEBadEncoding
  | CEBadLength
  | CENoIssuerKeys
  | CENotVerified
  deriving (Eq, Show)

codeErrorText :: CodeError -> Text
codeErrorText = \case
  CEBadPrefix -> "not a redemption code (expected " <> decodeLatin1 codePrefix <> "…)"
  CEBadEncoding -> "code is not valid base64url"
  CEBadLength -> "code is the wrong length"
  CENoIssuerKeys -> "no issuer keys are configured in this build, so no code can be accepted"
  CENotVerified -> "code did not verify under any known issuer key"

codePrefix :: ByteString
codePrefix = "SMPX1-"

-- | The signed message is nothing but a random nonce the holder generates and
-- blinds. There is nothing else to put in it: the issuer cannot read what it
-- signs, so anything here would be holder-controlled. The scheme version lives
-- in the code's prefix, and everything else comes from the key.
msgLen, sigLen :: Int
msgLen = 32
sigLen = 256

-- | RFC 9474 RSABSSA-SHA384-PSS: a blind-signed token verifies as an ordinary
-- RSA-PSS signature, which is why the client needs no blinding machinery.
pssParams :: PSS.PSSParams H.SHA384 ByteString ByteString
pssParams = PSS.defaultPSSParams H.SHA384

-- | Production keys are **deliberately undefined**: the format is fixed so that
-- adding them later is data, not code. Until then a release build can verify no
-- code at all, which is correct while nothing ships — and must say so plainly
-- rather than reporting an invalid code.
--
-- This is a /schedule/, not a key. Cohort keys are generated and published ahead
-- of time and the whole horizon is compiled in, because a cohort opened after an
-- app shipped could not otherwise be verified by it. The live set stays bounded —
-- a key retires when its cohort expires — but the schedule has to outrun the
-- update cycle. A build whose schedule has run out should say so, not report an
-- invalid code; the two are indistinguishable to the maths and completely
-- different to the user.
productionIssuerKeys :: [PinnedIssuerKey]
productionIssuerKeys = []

issuerKeys :: [PinnedIssuerKey]
issuerKeys = productionIssuerKeys
#if defined(dev_codes)
  <> [devIssuerKey]
#endif

encodeCode :: ByteString -> Text
encodeCode payload = decodeLatin1 codePrefix <> decodeLatin1 (b64u payload)

b64u :: ByteString -> ByteString
b64u = B.filter (/= 61) . BAE.convertToBase BAE.Base64URLUnpadded

verifyCode :: Text -> Either CodeError VerifiedCode
verifyCode code = do
  let raw = encodeUtf8 code
  body <-
    maybe (Left CEBadPrefix) Right $
      B.stripPrefix codePrefix raw
  payload <- either (const $ Left CEBadEncoding) Right (BAE.convertFromBase BAE.Base64URLUnpadded body :: Either String ByteString)
  if B.length payload /= msgLen + sigLen then Left CEBadLength else Right ()
  let (msg, sig) = B.splitAt msgLen payload
  if null issuerKeys then Left CENoIssuerKeys else Right ()
  case filter (\k -> PSS.verify pssParams (pikKey k) msg sig) issuerKeys of
    [] -> Left CENotVerified
    (k : _) ->
      Right
        VerifiedCode
          { vcMinLength = pikMinLength k,
            vcYears = pikYears k,
            vcExpires = pikExpires k,
            vcLabel = pikLabel k,
            vcNonce = msg
          }

#if defined(dev_codes)
-- | A fixed development keypair, compiled in only under the @dev_codes@ flag.
--
-- Fixed rather than generated so the codes a service prints are identical on
-- every run and tests can hardcode them. It is behind a **compile-time** flag
-- and not configuration on purpose: a dev verification key present in a release
-- build would be a forgery key for real codes, and no runtime setting should be
-- able to switch that on.
devIssuerKey :: PinnedIssuerKey
devIssuerKey =
  PinnedIssuerKey
    { pikLabel = "dev: 6+ letters, 2 years",
      pikKey = RSA.PublicKey {RSA.public_size = 256, RSA.public_n = devN, RSA.public_e = 65537},
      pikMinLength = 6,
      pikYears = 2,
      pikExpires = devExpiry
    }

-- | One cohort expiry for every dev code, mirroring how a real batch works.
devExpiry :: UTCTime
devExpiry = posixSecondsToUTCTime 1814400000 -- 2027-07-01

devIssuerPrivate :: RSA.PrivateKey
devIssuerPrivate =
  RSA.PrivateKey
    { RSA.private_pub = RSA.PublicKey {RSA.public_size = 256, RSA.public_n = devN, RSA.public_e = 65537},
      RSA.private_d = devD,
      RSA.private_p = 0,
      RSA.private_q = 0,
      RSA.private_dP = 0,
      RSA.private_dQ = 0,
      RSA.private_qinv = 0
    }

devN :: Integer
devN = 24888112474209384241822313289174761916432671221638604649518925371588661023857211328354011559848717830344726834019769246154626947666703992055350752697517435418048419727601941633666940975633179816697744805524303719572333648883192553542938468476238864154888444432850134036529175415793321307984568003216958478506050686177368523811693515246798542977965199174188430024788691021144684872571714513836211557103158962740151489815774309113725042504550173516616821401055947805896340622659904171191347695460979153885704616386392128700095744751859356877279249104214401084821283607493149034883118538479930388213794952102331084309127

devD :: Integer
devD = 3358567934478352598298312994636031469077475994997815272599377084491663000976443489753313063388651608886106537071743278041282340130984483661710515538655174921604898363838924146789606268039425703020810459130825977629395895306818361284980206832840327060833321674231755884753101719292554337974205707012081431617326396158749434571441235491401247111550429755050472715446167871053180426390130486145135280511231007956406465731980033447070053515891501760810730497736305620422623652969543717287523710529676336869267877915732532425513343594169679516878896078562472320280123731386736868075945531041237029017927929994367890859745

-- | Mint a development code by signing it directly, unblinded.
--
-- This exercises the /verification/ path exactly: an RFC 9474 blind-signed token
-- verifies as an ordinary RSA-PSS signature, so what the client does with a code
-- from here is byte-identical to what it does with a real one.
--
-- __It is not the production issuance model, and must not be copied as one.__
-- Here the issuer picks the nonce and sees the finished code, so it could match
-- a redemption back to whoever it issued to. Real issuance is the other way
-- round: the /holder/ generates the message, blinds it, and the issuer signs a
-- value it cannot read (RFC 9474 §5). That is what makes recording the nonce at
-- redemption safe — the issuer has never seen it, so the nullifier links to
-- nothing.
--
-- A production issuer therefore needs the blinding protocol; this function is a
-- local shortcut for a build that already trusts a published key.
-- | @nonce@ stands in for the value a holder would generate and blind. Passing
-- it in keeps dev codes deterministic: the same nonce always yields the same
-- code, so tests hardcode them instead of scraping stdout.
signDevCode :: ByteString -> IO (Either RSA.Error Text)
signDevCode nonce = do
  let msg = B.take msgLen (nonce <> B.replicate msgLen 0)
  fmap (\sig -> encodeCode (msg <> sig)) <$> PSS.signSafer pssParams devIssuerPrivate msg

#endif
