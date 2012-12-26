module Main where

import Data.Conduit
import Data.Conduit.Binary
import Data.Conduit.List as CL
import Data.Conduit.Text
import Data.Text as T
import Data.HashMap.Strict as M
import Data.Set as S
import Data.Char as C

import Control.Monad
import Control.Monad.Trans
import System.Environment
import Debug.Trace 

main = do
    [fp] <- getArgs 
    runResourceT $ 
        sourceFile fp $= decode utf8
                      $= CL.map (T.filter isvalid)
                      $$ byN 3 =$= collect M.empty 
                      =$ (await >>= lift . lift . print)
    where
      isvalid c = S.member (C.toLower c) elems

      elems = S.fromList $ ['a'..'z'] ++ ['а'..'я'] ++ " ,.!?-"
      
      byN n = do
        mx <- await
        case mx of 
          Nothing -> return ()
          Just x  -> 
            let t' = T.take n x
            in do
              yield t'
              when (T.length x > 1) $ do
                leftover $ T.drop 1 x
                byN n
    
      collect e = do
        mx <- await
        case mx of
          Nothing -> yield e
          Just x  -> collect (M.insertWith (+) x 1 e)

