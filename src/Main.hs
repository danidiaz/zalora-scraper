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
import           Data.List
import           System.IO
import           Prelude hiding (mapM_)

import           System.IO.Streams (InputStream, OutputStream, stdout)
import qualified System.IO.Streams as Streams
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding
import           Data.Text.IO as TIO

import           Data.Attoparsec.Text hiding (takeWhile)
import qualified Data.Attoparsec.Text as A
import           Data.Attoparsec.Combinator

import           Text.HTML.TagSoup

import           Network.Http.Client

matches :: Tag Text -> Tag Text -> Bool
matches pattern tag = tag ~== pattern

parseCG :: Parser [Text] 
parseCG = manyTill' (takeTill isEndOfLine *> endOfLine)
                    (_s_ string "wt.contentGroup" *> s_ char '=' *> char '{') 
          *> sepBy' (_s_ skipMany1 digit *> s_ char ':' *> parseQuot) 
                    (_s char ',')
    where s_ f x = f x <* skipSpace
          _s f x = skipSpace *> f x
          _s_ f x = skipSpace *> f x <* skipSpace
          parseQuot = char '"' *> A.takeWhile isAlpha <* char '"'

keywordList :: [Tag Text] -> Maybe [Text]
keywordList tags = listToMaybe $ do
    (_:TagText txt:_) <- partitions (matches $ TagOpen "script" []) tags
    maybeToList . maybeResult $ takeWhile (not.T.null) <$> parse parseCG txt

main :: IO ()
main = do
    soup <- parseTags . decodeUtf8 <$> get "http://www.zalora.sg/" concatHandler'
    mapM_ (mapM_ TIO.putStrLn) (keywordList soup)
    return ()







