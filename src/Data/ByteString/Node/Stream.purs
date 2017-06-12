module Data.ByteString.Node.Stream where

import Prelude

import Node.Stream as S
import Data.ByteString (ByteString)
import Data.ByteString.Node.Buffer as NB
import Data.Either (Either)

readChunk :: S.Chunk -> Either String ByteString
readChunk = map NB.fromBuffer <<< S.readChunk

