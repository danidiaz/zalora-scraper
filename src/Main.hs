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
import qualified Data.Set as S
import qualified Data.Map as M
import           Data.List
import           System.IO
import           Prelude hiding (mapM_)

import           System.IO.Streams (InputStream, OutputStream, stdout)
import qualified System.IO.Streams as Streams
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding
import           Data.Text.IO as TIO

import           Pipes
import           Pipes.Core

import           Data.Attoparsec.Text hiding (takeWhile)
import qualified Data.Attoparsec.Text as A
import           Data.Attoparsec.Combinator

import           Text.HTML.TagSoup

import           Network.Http.Client

matches :: Tag Text -> Tag Text -> Bool
matches pattern tag = tag ~== pattern

-- wt.contentId = "shop.pc.brand.mexx"
parseKeywords :: Parser [Text] 
parseKeywords = 
       manyTill' (takeTill isEndOfLine *> endOfLine)
                 (string "wt.contentId" *> _char '=' *> _char '"') 
    *> sepBy' (takeTill $ \c -> c == '.' || c == '"') (char '.')
    where _char x = skipSpace *> char x 

scrapeKeywords :: [Tag Text] -> Maybe [Text]
scrapeKeywords tags = listToMaybe $ do
    (_:TagText txt:_) <- partitions (matches $ TagOpen "script" []) tags
    maybeToList . maybeResult $ 
        takeWhile (not.T.null) <$> parse parseKeywords txt

scrapeLinks :: [Tag Text] -> [Text]
scrapeLinks =  filter (not . T.isSuffixOf ".html")
             . filter (not . T.isPrefixOf "http")
             . map (fromAttrib "href") 
             . filter (matches $ TagOpen "a" [("href","")])

scrapeSKUs :: [Tag Text] -> [Text]
scrapeSKUs =  concatMap (maybeToList . T.stripPrefix ":" . snd . T.breakOn ":")
            . map (fromAttrib "id") 
            . filter (matches $ TagOpen "a" [("href",""),("class","itm-link"),("id","")])

pageServer :: Monad m => Text -> Server (S.Set Text) (M.Map Text [Tag Text]) m a 
pageServer baseURL = forever $ undefined

main :: IO ()
main = do
    soup <- parseTags . decodeUtf8 <$> get "http://www.zalora.sg/womens/clothing/shorts" concatHandler'
    mapM_ (mapM_ TIO.putStrLn) (scrapeKeywords soup)
    mapM_ TIO.putStrLn (scrapeLinks soup)
    mapM_ TIO.putStrLn (scrapeSKUs soup)
    return ()







