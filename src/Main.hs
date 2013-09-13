{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedStrings  #-}

module Main where

------------------------------------------------------------------------------
import           System.IO
import           Control.Applicative
import           Control.Monad 
import           Control.Monad.Trans
import qualified Control.Monad.State as S
import           Control.Concurrent.Async (mapConcurrently)
import           Control.Exception
import           Data.Char
import           Data.Maybe
import           Data.List
import           Data.Monoid
import qualified Data.ByteString.Lazy as L
import qualified Data.Foldable as F
import qualified Data.Set as S
import qualified Data.Map as M
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding
import           Data.Attoparsec.Text hiding (takeWhile)
import           Data.Attoparsec.Combinator
import           Pipes (Proxy,Pipe,await,yield,runEffect,(>->))
import           Pipes.Core (Server,request,respond,(>+>))
import           Pipes.Lift (runStateP)
import qualified Pipes.Prelude as P
import           Text.HTML.TagSoup ((~==),Tag(..),fromAttrib,partitions,parseTags)
import qualified Options.Applicative as O
import qualified Network.HTTP.Conduit as C
--import qualified System.Remote.Monitoring as M

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
scrapeLinks =  map (T.dropWhileEnd (=='/') . fst . T.breakOn "?")
             . filter (not . T.isSuffixOf ".html")
             . filter (T.isPrefixOf "/")
             . map (fromAttrib "href") 
             . filter (matches $ TagOpen "a" [("href","")])

scrapeSKUs :: [Tag Text] -> [Text]
scrapeSKUs =  concatMap (maybeToList . T.stripPrefix ":" . snd . T.breakOn ":")
            . map (fromAttrib "id") 
            . filter (matches $ TagOpen "a" [("href",""),("class","itm-link"),("id","")])

-- Receives lists of relative urls to download concurrently,
-- responds with a map of the corresponding tag soups. 
pageServer :: MonadIO m => Text -> [Text] -> Server [Text] (M.Map Text [Tag Text]) m a 
pageServer baseUrl urlBatch = do
    pages <- liftIO . flip mapConcurrently urlBatch $ \rel -> do
        catches (C.simpleHttp . T.unpack $ baseUrl <> "/" <> rel)
                [ Handler $ \e -> let _ = e::C.HttpException in return "" ]
    let decodedPages = map (either (const []) parseTags . decodeUtf8' . L.toStrict) 
                           pages 
    respond (M.fromList $ zip urlBatch decodedPages) >>= pageServer baseUrl

-- Asks the upstream to download and tagsoupify not-yet-visited pages,
-- and passes the resulting tag soups to the downstream.
-- Extracts links and updates the list of not-yet-visited pages.
-- The Proxy state is (visited urls,pending urls).
spider :: S.MonadState (S.Set Text,S.Set Text) s => 
          () -> 
          Proxy [Text] (M.Map Text [Tag Text]) () [Tag Text] s ()
spider () = do
    (pending,visited) <- S.get
    if S.null pending
        then return ()
        else do processedPages <- request $ S.toList pending
                let urls =  M.keysSet processedPages
                    pending' = S.difference pending urls
                    visited' = S.union visited urls 
                    links = mconcat . map (S.fromList.scrapeLinks) . M.elems $ processedPages 
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

-- If a tag soup contains a contentId, parse it.
-- If the contentID contains 4 components, 
-- extract the SKUs from the soup and pass them downstream.
scraper :: Monad m => Pipe [Tag Text] SKUBatch m a
scraper = forever $ do
    tags <- await
    case scrapeKeywords tags of   
        Just kws@[_,_,_,_] -> yield $ SKUBatch kws (scrapeSKUs tags)
        _ -> return ()

-- Maps requests going upstream in a Proxy.
mapReq :: Monad m => (b -> c) -> b -> Proxy c a b a m r
mapReq f b = request (f b) >>= respond >>= mapReq f    

main :: IO ()
main = do
    let parser = (,,) <$> O.argument O.str (O.metavar "URL") 
                      <*> O.argument O.str (O.metavar "OUTPUTFILE") 
                      <*> O.option (O.value 1 <> 
                                    O.showDefault <> 
                                    O.short 'c' <> 
                                    O.metavar "CONCURRENCY" <> 
                                    O.help "Level of concurrency")
    (url,file,concurrency) <- O.execParser $ O.info (O.helper <*> parser) O.fullDesc
   -- M.forkServer "localhost" 8000
    withFile file WriteMode $ \h -> do
        let logVisited = liftIO . putStrLn . (<>) "Visited: " . show . M.keys 
        (_,_) <- runEffect $ runStateP (S.singleton "", S.empty) $ 
                      pageServer (T.pack url) >+> 
                      P.generalize (P.chain logVisited) >+>
                      -- Throttle the requests arriving to the page server.
                      mapReq (Prelude.take concurrency) >+> 
                      spider >+> 
                      (P.generalize $ scraper >-> P.map show >-> P.toHandle h) $ ()
        return ()
    
