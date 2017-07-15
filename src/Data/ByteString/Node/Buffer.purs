module Data.ByteString.Node.Buffer
  ( toBuffer
  , fromBuffer
  ) where

import Prelude

import Control.Monad.Eff (Eff)

import Data.ArrayBuffer.Types (ArrayBuffer, ArrayView, Uint8Array)
import Data.ByteString (ByteString(..))
import Data.ByteString.Internal (Ptr(..), newPtr )

import Node.Buffer (Buffer)
import Unsafe.Coerce (unsafeCoerce)

-- | Buffer implement Uint8Array interface, so it's safe to turn it to Uint8Array.
bufferToUint8Array :: Buffer -> Uint8Array
bufferToUint8Array = unsafeCoerce

-- | Turn a ByteString to a Buffer, this should only be used when you want to save
-- | this packet to Node.js Api, eg file system
toBuffer :: forall r. ByteString -> Eff r Buffer
toBuffer (ByteString (Ptr a av) n l) = _toBuffer (a + n) l (arrayBuffer av)

-- | Turn a Buffer to a strict ByteString
fromBuffer :: Buffer -> ByteString
fromBuffer buf =
  let uint8 = bufferToUint8Array buf
  in ByteString (newPtr uint8) 0 ((typedArraytoRecord uint8).length)

foreign import _toBuffer :: forall r. Int -> Int -> ArrayBuffer -> Eff r Buffer

arrayBuffer :: Uint8Array -> ArrayBuffer
arrayBuffer = _.buffer <<< typedArraytoRecord

typedArraytoRecord :: forall a. ArrayView a -> { buffer :: ArrayBuffer, length :: Int }
typedArraytoRecord = unsafeCoerce
