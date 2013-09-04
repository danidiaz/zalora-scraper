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
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding
import           Data.Text.IO as TIO

import           Text.HTML.TagSoup

import           Network.Http.Client

replusify = msum . map return

matches :: Tag Text -> Tag Text -> Bool
matches pattern tag = tag ~== pattern

scripts :: [Tag Text] -> [Text]
scripts tags = observeAll $ do
    (_:TagText txt:_) <- replusify $ partitions (matches $ TagOpen "script"  []) tags
    let mark = "wt.contentGroup = {" 
    (_:txt':_) <- return $ T.splitOn mark txt
    -- guard $ T.isInfixOf mark txt
    return txt'

main :: IO ()
main = do
    soup <- parseTags . decodeUtf8 <$> get "http://www.zalora.sg/" concatHandler'
    -- mapM_ (putStrLn . show) soup
    let s = scripts soup 
    mapM_ TIO.putStrLn s







