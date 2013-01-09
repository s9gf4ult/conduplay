{-# LANGUAGE BangPatterns #-}

module Main where

import Data.Conduit
import Data.Conduit.Binary
import qualified Data.Conduit.List as CL
import Data.Conduit.Text
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Char as C
import Data.Monoid

import Control.Monad
import Control.Monad.Trans
import System.Environment
import Debug.Trace 

main = do
    args <- getArgs
    case args of
      [fp] -> do
        runResourceT $ 
          sourceFile fp $= decode utf8
          $= CL.map (T.filter isvalid)
          $$ byN 3 =$= collect M.empty 
          =$ (await >>= lift . lift . print)
    where
      isvalid c = S.member (C.toLower c) elems

      elems = S.fromList $ ['a'..'z'] ++ ['а'..'я'] ++ " ,.!?-"
      
      byN count = byN' T.empty count

      byN' !t !count
        | l >= count = do
          yield $ T.take count t
          byN' (T.drop count t) count
        | l > 0 = do
          a <- await
          case a of
            Nothing -> do
              yield t
              return ()
            Just b -> let
              d = count - l
              in do
                yield $ mappend t $ T.take d b
                let (res, rest) = yieldfrom (T.drop d b)
                mapM_ yield res
                byN' rest count
        | otherwise = do
          a <- await
          case a of
            Nothing -> return ()
            Just b -> do
              let (res, rest) = yieldfrom b
              mapM_ yield res
              byN' rest count

        where
          l = T.length t
          yieldfrom txt | T.length txt < count = ([], txt)
                        | otherwise = (res, rest)
            where
              res = map (\x -> T.take count $ T.drop (count * x) txt) [0..a-1]
              rest | b == 0 = T.empty
                   | otherwise = T.drop (count * a) txt
              (a, b) = divMod (T.length txt) count
          
    
      collect !e = do
        mx <- await
        case mx of
          Nothing -> yield e
          Just x  -> collect (M.insertWith (+) x 1 e)

