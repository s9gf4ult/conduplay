module Main where

import Control.Monad.Trans

import qualified Data.Text as T

import Control.Monad.Primitive
import System.IO
import System.Environment
import System.Random.MWC
import qualified System.Random.MWC.Monad as RM
import qualified Data.Vector.Unboxed as V

import Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Text as CT

smbls = V.fromList ['a'..'z']
smlen = (V.length smbls) - 1

gensym :: (PrimMonad m) => RM.Rand m Char
gensym = do
  l <- RM.uniformR (0, smlen)
  return $ smbls V.! l

main = do
  args <- getArgs
  case args of
    [a] -> do
      generate 300 $= isolate (read a :: Int) $= CT.encode CT.utf8 $$ CB.sinkHandle stdout


    _ -> print "bad arguments"

  where
    generate count = do
      g <- lift $ create
      ggen count g

    ggen count g = do
      d <- lift $ RM.runRand (sequence $ replicate count gensym) g
      yield $ T.pack d
      ggen count g
  
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
                      
      
      
