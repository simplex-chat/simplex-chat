module Simplex.Chat.Remote.Multicast (setMembership) where

import Foreign (Ptr, allocaBytes, castPtr, pokeByteOff)
import Foreign.C.Types (CInt (..))
import Network.Socket

#include <HsNet.h>

-- | Toggle multicast group membership.
-- NB: Group membership is per-host, not per-process. A socket is only used to access system interface for groups.
setMembership :: Socket -> HostAddress -> Bool -> IO (Either CInt ())
setMembership sock group membership = allocaBytes #{size struct ip_mreq} $ \mReqPtr -> do
  #{poke struct ip_mreq, imr_multiaddr} mReqPtr group
  #{poke struct ip_mreq, imr_interface} mReqPtr (0 :: HostAddress) -- attempt to contact the group on ANY interface
  withFdSocket sock $ \fd -> do
    rc <- c_setsockopt fd c_IPPROTO_IP flag (castPtr mReqPtr) (#{size struct ip_mreq})
    if rc == 0
      then pure $ Right ()
      else pure $ Left rc
  where
    flag = if membership then c_IP_ADD_MEMBERSHIP else c_IP_DROP_MEMBERSHIP

#ifdef mingw32_HOST_OS

foreign import stdcall unsafe "setsockopt"
    c_setsockopt :: CInt -> CInt -> CInt -> Ptr CInt -> CInt -> IO CInt

c_IP_ADD_MEMBERSHIP, c_IP_DROP_MEMBERSHIP :: CInt
c_IP_ADD_MEMBERSHIP  = 12
c_IP_DROP_MEMBERSHIP = 13

#else

foreign import ccall unsafe "setsockopt"
    c_setsockopt :: CInt -> CInt -> CInt -> Ptr CInt -> CInt -> IO CInt

c_IP_ADD_MEMBERSHIP, c_IP_DROP_MEMBERSHIP :: CInt
c_IP_ADD_MEMBERSHIP  = #const IP_ADD_MEMBERSHIP
c_IP_DROP_MEMBERSHIP = #const IP_DROP_MEMBERSHIP

#endif

c_IPPROTO_IP :: CInt
c_IPPROTO_IP = #const IPPROTO_IP
