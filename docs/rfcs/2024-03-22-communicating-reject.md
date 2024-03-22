# Communicating rejection

## Problem

Many interactions support either approval, or silent rejection. From privacy perspective, rejection being silent is a correct default. However, for improved usability we could add optional communication of rejection, as many users don't mind sending such signals to their contacts.

Features currently not supporting communicating rejection:
- Rejecting contact request
- Rejecting group join request
- Rejecting call
- Rejecting group invitation
- TBC Other?

## Solution

## Rejection of contact / group join requests

- Contact and group join requests are very similar between each other and different from other features as they both use mechanism of "contact connections".

- Rejection can be made to the address proposed by requester in AgentInvitation message, where currently AgentConfirmation is sent in case of acceptance.

  ``` haskell
  data AgentMsgEnvelope
    = ...
    | AgentReject
        { e2eEncryption :: RcvE2ERatchetParams 'C.X448,
          encRejectInfo :: ByteString
        }
  ```

- Unlike other AgentMsgEnvelope constructors, AgentReject doesn't require agentVersion since connection will be deleted after sending this message.

  - We may be able to re-use recently added mechanism of marking connection for deletion with deleted_at_wait_delivery field without much additional work.

- Both sync and async versions of agent functions are required, as contact rejection will be user action, while group join rejection will be automated (in case, for example, if link host is no longer admin).

  - Group requests non-automatic approval is a separate matter and requires UI consideration, but if it was added it would also use sync function.

  - For sync function either new API can be added, or rejectContact can be parameterized.

- Chat protocol requires adding new messages to be sent in encRejectInfo, to be processed on requester side based on connections' semantics.

  ```haskell
  -- / contact request rejection
  XReject :: ChatMsgEvent 'Json
  -- or
  -- (contact can send reason for rejection; we don't even have welcome messages in XContact though)
  XReject :: Maybe Text -> ChatMsgEvent 'Json

  -- / group join request rejection
  XGrpReject :: Maybe GrpRejectReason -> ChatMsgEvent 'Json

  data GrpRejectReason
    = GRRNone                -- manual reject with no reason? use Nothing in GRRText instead?
    | GRRCantInvite          -- e.g., no longer admin
    | GRRText {text :: Text} -- manual reject once supported? Maybe Text?

  -- add similar type for contact request rejection?
  -- minimal is to always send XReject without reasons
  ```

  - As a side note, it may have been a design mistake to mix both "connInfo" messages as well as regular chat messages in a single type for protocol messages, so it may be best to keep these as separate constructors.

- Versioning considerations:
  - Increase chat version.
  - We already save peer chat version on contact_requests on initial REQ message. It can be used to differentiate UI whether contact supports rejection messages and not offer option to reject with notification.
  - Increase agent version? Agent can prohibit sending AgentReject based on version in AgentInvitation, though it shouldn't be reachable as chat should also prohibit it.

## Rejecting calls

- New chat protocol message is enough
  - Based on user action in reply to XCallInv instead of XCallOffer.
  - Send in APIRejectCall.

  ```haskell
  XCallReject :: CallId -> ChatMsgEvent 'Json
  ```

- Same chat versioning considerations as above.

## Rejecting group invitation

- Same, new protocol message.
  - Based on user action in reply to XGrpInv instead of XGrpAcpt.
  - Can't be sent as simple chat message since there's no group ID in invitation? (only optional groupLinkId)
    - So, have to send as "conn info" via join, same as for XGrpAcpt.
  - APIDeleteChat is already used for deleting group invitations, can re-use. In this case `notify` parameter in APIDeleteChat can be used to send rejection.

  ```haskell
  XGrpReject :: CallId -> ChatMsgEvent 'Json
  ```

- Same chat versioning considerations as above.
