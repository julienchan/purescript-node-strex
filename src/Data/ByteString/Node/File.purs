module Data.ByteString.Node.File
  ( readFile
  , writeFile
  , appendFile
  , createWriteStreamStartWith
  , fdCreateWriteStreamStartWith
  , createReadableStreamRangeWith
  , fdCreateReadableStreamRangeWith
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error)

import Data.ByteString (ByteString)
import Data.ByteString.Node.Buffer (fromBuffer, toBuffer)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn2, Fn3, Fn4, runFn2, runFn4, runFn3)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toNullable)

import Node.FS.Internal (mkEff, unsafeRequireFS)
import Node.FS (FS, fileFlagsToNode, FileDescriptor)
import Node.FS.Perms as Perms
import Node.FS.Stream (WriteStreamOptions, ReadStreamOptions)
import Node.Path (FilePath())
import Node.Stream (Readable(), Writable())


type JSCallback a = Fn2 (Nullable Error) a Unit

foreign import handleCallbackImpl ::
  forall eff a. Fn3 (Error -> Either Error a)
                    (a -> Either Error a)
                    (Callback eff a)
                    (JSCallback a)

handleCallback :: forall eff a. (Callback eff a) -> JSCallback a
handleCallback cb = runFn3 handleCallbackImpl Left Right cb

fs ::
  { readFile :: forall a opts. Fn3 FilePath { | opts } (JSCallback a) Unit
  , writeFile :: forall a opts. Fn4 FilePath a { | opts } (JSCallback Unit) Unit
  , appendFile :: forall a opts. Fn4 FilePath a { | opts } (JSCallback Unit) Unit
  , createReadStream  :: forall eff opts. Fn2 (Nullable FilePath) { | opts } (Readable () (fs :: FS | eff))
  , createWriteStream :: forall eff opts. Fn2 (Nullable FilePath) { | opts } (Writable () (fs :: FS | eff))
  }
fs = unsafeRequireFS

-- | Type synonym for callback functions.
type Callback eff a = Either Error a -> Eff (fs :: FS | eff) Unit

readFile :: forall eff. FilePath -> Callback eff ByteString -> Eff (fs :: FS | eff) Unit
readFile fp cb = mkEff \_ -> runFn3
  fs.readFile fp {} (handleCallback (cb <<< map fromBuffer))

writeFile :: forall eff. FilePath -> ByteString -> Callback eff Unit -> Eff (fs :: FS | eff) Unit
writeFile fp bs cb = do
  buff <- toBuffer bs
  mkEff \_ -> runFn4 fs.writeFile fp buff {} (handleCallback cb)

appendFile :: forall eff. FilePath -> ByteString -> Callback eff Unit -> Eff (fs :: FS | eff) Unit
appendFile fp bs cb = do
  buff <- toBuffer bs
  mkEff \_ -> runFn4 fs.appendFile fp buff {} (handleCallback cb)

createWriteStreamStartWith
  :: forall eff
   . WriteStreamOptions
  -> Int
  -> FilePath
  -> Eff (fs :: FS | eff) (Writable () (fs :: FS | eff))
createWriteStreamStartWith opts start file = mkEff $ \_ -> runFn2
  fs.createWriteStream (nonnull file)
    { mode: Perms.permsToInt opts.perms
    , flags: fileFlagsToNode opts.flags
    , start
    }

fdCreateWriteStreamStartWith
  :: forall eff
   . WriteStreamOptions
  -> Int
  -> FileDescriptor
  -> Eff (fs :: FS | eff) (Writable () (fs :: FS | eff))
fdCreateWriteStreamStartWith opts start fd = mkEff $ \_ -> runFn2
  fs.createWriteStream null
    { fd
    , mode: Perms.permsToInt opts.perms
    , flags: fileFlagsToNode opts.flags
    , start
    }

createReadableStreamRangeWith
  :: forall eff
   . ReadStreamOptions
  -> Int
  -> Int
  -> FilePath
  -> Eff (fs :: FS | eff) (Readable () (fs :: FS | eff))
createReadableStreamRangeWith opts start end file = mkEff $ \_ -> runFn2
  fs.createReadStream (nonnull file)
    { mode: Perms.permsToInt opts.perms
    , flags: fileFlagsToNode opts.flags
    , autoClose: opts.autoClose
    , start
    , end
    }

fdCreateReadableStreamRangeWith
  :: forall eff
   . ReadStreamOptions
  -> Int
  -> Int
  -> FileDescriptor
  -> Eff (fs :: FS | eff) (Readable () (fs :: FS | eff))
fdCreateReadableStreamRangeWith opts start end fd = mkEff $ \_ -> runFn2
  fs.createReadStream null
    { fd
    , mode: Perms.permsToInt opts.perms
    , flags: fileFlagsToNode opts.flags
    , autoClose: opts.autoClose
    , start
    , end
    }

null :: forall a. Nullable a
null = toNullable Nothing

nonnull :: forall a. a -> Nullable a
nonnull = toNullable <<< Just