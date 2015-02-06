{-# LANGUAGE ForeignFunctionInterface, DeriveDataTypeable #-}

module Sound.VAD.WebRTC
  ( VAD(), VADException(..)
  , create
  , Sound.VAD.WebRTC.free
  , Sound.VAD.WebRTC.init
  , Aggressiveness, setMode
  , process
  , validRateAndFrameLength
  ) where

import Control.Applicative
import Control.Exception
import Control.Monad
import qualified Data.ByteString as BS
import Data.Int
import Data.Typeable
import Foreign
import Foreign.C

newtype VADException = VADException String
  deriving (Show, Typeable)

instance Exception VADException

orDie :: IO CInt -> String -> IO ()
orDie m err = do
  res <- m
  unless (res == 0) $ throwIO $ VADException err

data VadInst

newtype VAD = VAD (Ptr VadInst)

foreign import ccall unsafe "webrtc_vad.h WebRtcVad_Create" _WebRtcVad_Create :: Ptr (Ptr VadInst) -> IO CInt

create :: IO VAD
create = alloca $ \ptr -> do
  _WebRtcVad_Create ptr `orDie` "Could not create VAD instance."
  VAD <$> peek ptr

-- we can use the foreign import directly
foreign import ccall unsafe "webrtc_vad.h WebRtcVad_Free" free :: VAD -> IO ()

foreign import ccall unsafe "webrtc_vad.h WebRtcVad_Init" _WebRtcVad_Init :: VAD -> IO CInt

init :: VAD -> IO ()
init vad
  = _WebRtcVad_Init vad `orDie` "Could not initialize VAD instance - NULL pointer or Default mode could not be set."

foreign import ccall unsafe "webrtc_vad.h WebRtcVad_set_mode" _WebRtcVad_set_mode :: VAD -> CInt -> IO CInt

type Aggressiveness = Int

setMode :: Aggressiveness -> VAD -> IO ()
setMode aggr vad = do
  _WebRtcVad_set_mode vad (fromIntegral aggr) `orDie` "NULL pointer, mode could not be set or the VAD instance has not been initialized"

foreign import ccall unsafe "webrtc_vad.h WebRtcVad_Process" _WebRtcVad_Process :: VAD -> CInt -> Ptr Int16 -> CInt -> IO CInt

process :: Int -> BS.ByteString -> VAD -> IO Bool
process sampleRate buffer vad = do
  res <- BS.useAsCStringLen buffer $ \( ptr, len ) ->
    _WebRtcVad_Process vad (fromIntegral sampleRate) (castPtr ptr) (fromIntegral (len `div` 2))
  case res of
    1 -> return True
    0 -> return False
    -1 -> throwIO $ VADException "Error while processing buffer."

foreign import ccall unsafe "webrtc_vad.h WebRtcVad_ValidRateAndFrameLength" _WebRtcVad_ValidRateAndFrameLength :: CInt -> CInt -> CInt

validRateAndFrameLength :: Int -> Int -> Bool
validRateAndFrameLength rate frameLength
  = case _WebRtcVad_ValidRateAndFrameLength (fromIntegral rate) (fromIntegral frameLength) of
      0 -> True
      -1 -> False