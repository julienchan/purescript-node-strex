{-
  This module wrapper around Node.Stream, so we can use ByteString directly to Node.js
  Stream Api.
-}

module Data.ByteString.Node.Stream
  ( onData
  , onDataEither
  , read
  , readEither
  , write
  , module Node.Stream
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)

import Data.ByteString (ByteString)
import Data.ByteString.Node.Buffer as NB
import Data.Either (Either)
import Data.Maybe (Maybe)

import Node.Stream
  ( Stream(), Read(), Readable(), Write(), Writable(), Duplex(), onDataString
  , setEncoding, onReadable, onEnd, onFinish, onClose, onError, resume, pause
  , isPaused, pipe, readString, writeString, cork, uncork, setDefaultEncoding, end)
import Node.Stream as S

onData
  :: forall w eff
   . S.Readable w (exception :: EXCEPTION | eff)
  -> (ByteString -> Eff (exception :: EXCEPTION | eff) Unit)
  -> Eff (exception :: EXCEPTION | eff) Unit
onData s f = S.onData s (f <<< NB.fromBuffer)

onDataEither
  :: forall r eff
   . S.Readable r (exception :: EXCEPTION | eff)
  -> (Either String ByteString -> Eff (exception :: EXCEPTION | eff) Unit)
  -> Eff (exception :: EXCEPTION | eff) Unit
onDataEither r cb = S.onDataEither r (cb <<< map NB.fromBuffer)

read
  :: forall w eff
   . S.Readable w (exception :: EXCEPTION | eff)
   -> Maybe Int
   -> Eff (exception :: EXCEPTION | eff) (Maybe ByteString)
read s = map (map NB.fromBuffer) <<< S.read s

readEither
  :: forall w eff
   . S.Readable w eff
  -> Maybe Int
  -> Eff eff (Maybe (Either String ByteString))
readEither s = map (map (map NB.fromBuffer)) <<< S.readEither s

write
  :: forall r eff
   . S.Writable r eff
  -> ByteString
  -> Eff eff Unit
  -> Eff eff Boolean
write s bs finisher = do
  buf <- NB.toBuffer bs
  S.write s buf finisher
