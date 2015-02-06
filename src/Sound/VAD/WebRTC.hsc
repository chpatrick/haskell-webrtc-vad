{-# LANGUAGE ForeignFunctionInterface, DeriveDataTypeable #-}

module Sound.VAD.WebRTC
  ( VAD(), VADException(..)
  , create
  ) where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Typeable
import Foreign
import Foreign.C

newtype VADException = VADException String
  deriving (Show, Typeable)

instance Exception VADException

data VadInst

newtype VAD = VAD (Ptr VadInst)

foreign import ccall "webrtc_vad.h WebRtcVad_Create" _WebRtcVad_Create :: Ptr (Ptr VadInst) -> IO CInt

create :: IO VAD
create = alloca $ \ptr -> do
  res <- _WebRtcVad_Create ptr
  when (res /= 0) $ throwIO $ VADException "Could not create VAD instance."
  VAD <$> peek ptr