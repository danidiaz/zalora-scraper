{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedStrings  #-}

module Main where

------------------------------------------------------------------------------
import           System.IO
import           Control.Applicative
import           Control.Monad 
import           Control.Monad.Trans
import qualified Control.Monad.State as S
import           Control.Concurrent.Async
import           Data.Char
import           Data.Maybe
import           Data.List
import           Data.Monoid
import qualified Data.Foldable as F
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Text as T
import           Data.Text.Encoding
import           Data.Attoparsec.Text hiding (takeWhile)
import           Data.Attoparsec.Combinator
import           Data.Text (Text)
import           Pipes
import           Pipes.Core
import           Pipes.Lift
import qualified Pipes.Prelude as P
import           Text.HTML.TagSoup
import           Network.Http.Client
import qualified Options.Applicative as O

matches :: Tag Text -> Tag Text -> Bool
matches pattern tag = tag ~== pattern

parseKeywords :: Parser [Text] 
parseKeywords = 
       manyTill' (takeTill isEndOfLine *> endOfLine)
                 (_string "wt.contentId" *> _char '=' *> _char '"') 
    *> sepBy' (takeTill $ \c -> c == '.' || c == '"') (char '.')
    where _char x = skipSpace *> char x 
          _string x = skipSpace *> string x 

scrapeKeywords :: [Tag Text] -> Maybe [Text]
scrapeKeywords tags = listToMaybe $ do
    (_:TagText txt:_) <- partitions (matches $ TagOpen "script" []) tags
    maybeToList . maybeResult $ 
        takeWhile (not.T.null) <$> parse parseKeywords txt

scrapeLinks :: [Tag Text] -> [Text]
scrapeLinks =  map (T.toLower . T.dropWhileEnd (=='/') . fst . T.breakOn "?")
             . filter (not . T.isSuffixOf ".html")
             . filter (T.isPrefixOf "/")
             . map (fromAttrib "href") 
             . filter (matches $ TagOpen "a" [("href","")])

scrapeSKUs :: [Tag Text] -> [Text]
scrapeSKUs =  concatMap (maybeToList . T.stripPrefix ":" . snd . T.breakOn ":")
            . map (fromAttrib "id") 
            . filter (matches $ TagOpen "a" [("href",""),("class","itm-link"),("id","")])

pageServer :: MonadIO m => Text -> [Text] -> Server [Text] (M.Map Text [Tag Text]) m a 
pageServer baseUrl urlBatch = do
    pages <- liftIO . flip mapConcurrently urlBatch $ \rel ->
                parseTags . decodeUtf8 <$> get (encodeUtf8 $ baseUrl <> "/" <> rel) concatHandler'
    respond (M.fromList $ zip urlBatch pages) >>= pageServer baseUrl

spider :: S.MonadState (S.Set Text,S.Set Text) s => 
                   () -> Proxy [Text] (M.Map Text [Tag Text]) () [Tag Text] s ()
spider () = do
    (pending,visited) <- S.get
    if S.null pending
        then return ()
        else do processedPages <- request $ S.toList pending
                let urls =  M.keysSet processedPages
                    pending' = S.difference pending urls
                    visited' = S.union visited urls 
                    links = M.foldl' (\s -> S.union s . S.fromList . scrapeLinks) S.empty processedPages 
                    pending'' = S.union pending' (S.difference links visited')
                S.put (pending'',visited')
                F.forM_ processedPages respond 
                spider ()

data SKUBatch = SKUBatch {
        _keywords :: [Text],
        _skus :: [Text] 
    } 

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

mapReq :: Monad m => (b -> c) -> b -> Proxy c a b a m r
mapReq f b = request (f b) >>= respond >>= mapReq f    

main :: IO ()
main = do
    let parser = (,,) <$> O.argument O.str (O.metavar "URL") 
                      <*> O.argument O.str (O.metavar "OUTPUTFILE") 
                      <*> O.argument O.auto (O.metavar "CONCURRENCY") 
    (url,file,concurrency) <- O.execParser (O.info parser mempty)
    withFile file WriteMode $ \h -> do
        let logVisited = liftIO . putStrLn . (<>) "Visited: " . show . M.keys 
        (_,_) <- runEffect $ runStateP (S.singleton "", S.empty) $ 
                      pageServer (T.pack url) >+> 
                      P.generalize (P.chain logVisited) >+> 
                      mapReq (Prelude.take concurrency) >+>
                      spider >+> 
                      (P.generalize $ scraper >-> P.map show >-> P.toHandle h) $ ()
        return ()
    
