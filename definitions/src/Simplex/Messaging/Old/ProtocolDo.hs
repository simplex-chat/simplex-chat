{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Simplex.Messaging.Old.ProtocolDo where

import Simplex.Messaging.Old.Protocol

-- redifine Monad operators to compose commands
-- using `do` notation with RebindableSyntax extension
(>>=) :: Command a b from1 to1 s1 s2 ss1 ss2 n1 n2
      -> (b -> Command b c from2 to2 s2 s3 ss2 ss3 n2 n3)
      -> Command a c from1 to2 s1 s3 ss1 ss3 n1 n3
(>>=) = (:>>=)

(>>)  :: Command a b from1 to1 s1 s2 ss1 ss2 n1 n2
      -> Command c d from2 to2 s2 s3 ss2 ss3 n2 n3
      -> Command a d from1 to2 s1 s3 ss1 ss3 n1 n3
(>>) = (:>>)

fail :: String -> Command a String from to state (None <==> None <==| None) ss ss n n
fail = Fail
