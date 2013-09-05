{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

------------------------------------------------------------------------------
import           Control.Exception (SomeException, try)
import           Control.Applicative
import           Control.Monad hiding (mapM_)
import           Control.Monad.Trans
import           Control.Monad.Logic (observeAll)
import           Data.Foldable (mapM_)
import           Data.Traversable
import           Data.Char
import           Data.Maybe
import           Data.Monoid
import           System.IO
import           Prelude hiding (takeWhile,mapM_)

import           System.IO.Streams (InputStream, OutputStream, stdout)
import qualified System.IO.Streams as Streams
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding
import           Data.Text.IO as TIO

import           Data.Attoparsec.Text
import           Data.Attoparsec.Combinator

import           Text.HTML.TagSoup

import           Network.Http.Client

replusify = msum . map return

matches :: Tag Text -> Tag Text -> Bool
matches pattern tag = tag ~== pattern

parseCG :: Parser [Text] 
parseCG = 
    manyTill' anyChar (string "wt.contentGroup = {") *> sepBy' entry (char ',')
    where
        ss = skipSpace
        entry =  ss *> skipMany digit *> ss *> char ':' *> ss 
                        *> char '"' *> takeWhile isAlpha <* char '"'

keywordList :: [Tag Text] -> Maybe [Text]
keywordList tags = listToMaybe . observeAll $ do
    (_:TagText txt:_) <- replusify $ partitions (matches $ TagOpen "script" []) tags
    replusify . maybeToList . maybeResult $ parse parseCG txt

main :: IO ()
main = do
    soup <- parseTags . decodeUtf8 <$> get "http://www.zalora.sg/" concatHandler'
    mapM_ (mapM_ TIO.putStrLn) (keywordList soup)
    return ()







