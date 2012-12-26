module Main where

import Blaze.ByteString.Builder
import qualified Blaze.ByteString.Builder.Char.Utf8 as BU
import qualified Data.ByteString.Lazy as BL
import System.Random
import System.Environment
import System.Random.Mersenne.Pure64
import System.IO


main = do
  args <- getArgs
  case args of
    [a] -> do 
      g <- newPureMT
      let b = BU.fromString $ take (read a) $ randomRs ('a', 'z') g
      BL.putStr $ toLazyByteString $ b
  
    _ -> print "bad arguments"

