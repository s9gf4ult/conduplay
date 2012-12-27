module Main where

import Control.Monad.Trans

import qualified Data.Text as T

import System.IO (stdout)
import System.Environment (getArgs)
  
import Control.Monad.Mersenne.Random
import System.Random.Mersenne.Pure64

import Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Text as CT

import qualified Data.Vector.Unboxed as UV

  

main = do
  args <- getArgs
  case args of
    [a] -> do
      generate 1000 $= isolate (read a :: Int) $= CT.encode CT.utf8 $$ CB.sinkHandle stdout


    _ -> print "bad arguments"

  where
    generate count = do
      g <- lift $ newPureMT
      ggen count g

    ggen count g = do
      let (d, gg) = runRandom (rnds count) g
      yield $ T.pack d
      ggen count gg
      

    rnds count = do
      ints <- sequence $ replicate count getInt
      return $ map i2char ints

    i2char i = chars UV.! (i `mod` charlen)

    chars = UV.fromList ['A'..'Z']
    charlen = UV.length chars
  
    isolate count = isolate' 0 count
    isolate' acc count | acc > count = return ()
                       | otherwise = do
      a <- await
      case a of
        Nothing -> return ()
        Just b -> let l = T.length b
                  in do
                    if (acc + l <= count)
                      then do
                        yield b
                        isolate' (acc + l) count
                      else do
                        yield $ T.take (count - acc) b
                        isolate' (acc + l) count
