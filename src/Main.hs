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
import           Control.Lens
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
import qualified Data.Text.IO as TIO

import           Pipes
import           Pipes.Core
import qualified Pipes.Prelude as P

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

pageServer :: Monad m => Text -> [Text] -> Server [Text] [(Text,[Tag Text])] m a 
pageServer baseURL urlBatch = 
    respond [] >>= pageServer baseURL

data SKUBatch = SKUBatch {
        _keywords :: [Text],
        _skus :: [Text] 
    } 

makeLenses ''SKUBatch

instance Show SKUBatch where
    show (SKUBatch ks skus) = 
        let col1 = concat ["\"",concat . intersperse "," . map T.unpack $ skus,"\""]
            col2 = concat $ intersperse "." . map T.unpack $ ks
        in concat  [col1, ", ", col2]

scraperCore :: Monad m => Text -> () -> Proxy [Text] [(Text,[Tag Text])] () SKUBatch m ()
scraperCore urls () = do
    processedPages <- request []
    respond $ SKUBatch [] []

printer :: MonadIO m => Consumer SKUBatch m a
printer = forever $ await >>= liftIO . putStrLn . show

pipeline :: MonadIO m => Text -> Text -> Effect m ()
pipeline baseUrl initialUrl = pageServer baseUrl >+> scraperCore initialUrl >+> P.generalize printer $ ()

main :: IO ()
main = do
    soup <- parseTags . decodeUtf8 <$> get "http://www.zalora.sg/womens/clothing/shorts" concatHandler'
    mapM_ (mapM_ TIO.putStrLn) (scrapeKeywords soup)
    mapM_ TIO.putStrLn (scrapeLinks soup)
    mapM_ TIO.putStrLn (scrapeSKUs soup)
    runEffect $ pipeline "http://www.zalora.sg/" "/" 
    putStrLn $ show $ SKUBatch ["shop","sg","bands","foo"] ["ADFBCD","4343434","4343344334"]
    return ()

