{-# LANGUAGE CPP                #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedStrings  #-}

module Main where

------------------------------------------------------------------------------
import           Control.Exception (SomeException, try)
import           Control.Applicative
import           Control.Monad hiding (mapM_)
import           Control.Monad.Trans
import qualified Control.Monad.State as S
import qualified Control.Monad.Reader as R
import           Control.Monad.Logic (observeAll)
import           Control.Concurrent.Async
import           Control.Lens
import qualified Data.Foldable as F
import qualified Data.Traversable as TR
import           Data.Char
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set as S
import qualified Data.Map as M
import           Data.List
import           System.IO
import           Prelude hiding (mapM_)

import           System.IO
import           System.IO.Streams (InputStream, OutputStream, stdout)
import qualified System.IO.Streams as Streams
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding
import qualified Data.Text.IO as TIO

import           Pipes
import           Pipes.Core
import           Pipes.Lift
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

pageServer :: (MonadIO m, R.MonadReader Text m) => [Text] -> Server [Text] (M.Map Text [Tag Text]) m a 
pageServer urlBatch = do
    rootUrl <- R.ask
    pages <- liftIO . flip mapConcurrently urlBatch $ \rel ->
                parseTags . decodeUtf8 <$> get (encodeUtf8 $ rootUrl <> "/" <> rel) concatHandler'
    respond (M.fromList $ zip urlBatch pages) >>= pageServer

urlLogger :: MonadIO m => Pipe (M.Map Text a) (M.Map Text a) m r
urlLogger = forever $ do
    stuff <- await  
    liftIO $ putStrLn $ "Visited urls: " <> (show . F.toList . M.keysSet $ stuff)
    yield stuff

data SKUBatch = SKUBatch {
        _keywords :: [Text],
        _skus :: [Text] 
    } 

throttler :: R.MonadReader Int m => [Text] -> Proxy [Text] a [Text] a m r
throttler urls = do
    batchSize <- R.ask
    request (Prelude.take batchSize urls) >>= respond >>= throttler

spider :: S.MonadState (S.Set Text,S.Set Text) s => 
                   () -> Proxy [Text] (M.Map Text [Tag Text]) () [Tag Text] s ()
spider () = do
    (pending,visited) <- S.get
    if S.null pending
        then return ()
        else do processedPages <- request $ S.toList pending
                let pending' = S.difference pending (M.keysSet processedPages)
                    links = M.foldl' (\s -> S.union s . S.fromList . scrapeLinks) S.empty processedPages 
                    pending'' = S.union pending' (S.difference links visited)
                    visited' = S.union links visited 
                S.put (pending'',visited')
                F.forM_ processedPages respond 
                spider ()

makeLenses ''SKUBatch

instance Show SKUBatch where
    show (SKUBatch ks skus) = 
        let col1 = concat ["\"",concat . intersperse "," . map T.unpack $ skus,"\""]
            col2 = concat $ intersperse "." . map T.unpack $ ks
        in concat  [col1, "; ", col2]

scraper :: Monad m => Pipe [Tag Text] SKUBatch m a
scraper = forever $ do
    tags <- await
    case scrapeKeywords tags of   
        Just kws@[_,_,_,_] -> yield $ SKUBatch kws (scrapeSKUs tags)
        _ -> return ()

printer :: (R.MonadReader Handle m, MonadIO m) => Consumer SKUBatch m a
printer = forever $ do
    handle <- R.ask
    await >>= liftIO . hPutStrLn handle . show

main :: IO ()
main = do
    let configuration = ("http://www.zalora.sg/",3,System.IO.stdout)
    withFile "./dist/result.txt" WriteMode $ \h -> 
        let ran = runRWSP (_3 .~ h $ configuration) (S.singleton "/", S.empty) $ 
                      hoist (magnify _1) . pageServer >+> 
                      P.generalize urlLogger >+> 
                      hoist (magnify _2) . throttler >+> 
                      spider >+> 
                      P.generalize scraper >+> 
                      hoist (magnify _3) . P.generalize printer $ ()
        in do (_,_,()) <- runEffect ran 
              return ()
