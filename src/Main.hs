{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

------------------------------------------------------------------------------
import           Control.Exception (SomeException, try)
import           Control.Applicative
import           Control.Monad hiding (mapM_)
import           Control.Monad.Trans
import           Control.Monad.Logic 
import           Data.Foldable ()
import           Data.Traversable
import           System.IO
import           Prelude

import           System.IO.Streams (InputStream, OutputStream, stdout)
import qualified System.IO.Streams as Streams
import qualified Data.ByteString as S

import           Text.HTML.TagSoup

import           Network.Http.Client


matches :: Tag S.ByteString -> Tag S.ByteString -> Bool
matches pattern tag = tag ~== pattern

scripts :: [Tag S.ByteString] -> [S.ByteString]
scripts tags = observeAll $ do
    (_:TagText txt:_) <- msum . map return $ partitions (matches $ TagOpen "script"  []) tags
    return txt


main :: IO ()
main = do
    soup <- parseTags <$> get "http://www.zalora.sg/" concatHandler'
    -- mapM_ (putStrLn . show) soup
    let s = scripts soup 
    mapM_ S.putStrLn s
