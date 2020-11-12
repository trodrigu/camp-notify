{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Backend where

import Common.Route
import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Reader
import Data.Maybe
import Data.Text
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP
import qualified Network.HTTP.Types.Header as HTTP
import Obelisk.Backend
import Text.HTML.Scalpel
import Control.Concurrent.Thread.Delay
import Data.Time.Clock
import Control.Concurrent.ParallelIO

-- Database Persistence
share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
StatePark
  name Text
  nameSlug Text
  Primary nameSlug
  link Text
  hasReservation Bool
  lastSync UTCTime
  deriving Show
|]

data ScrapedStatePark =
  ScrapedStatePark
    { name :: Text
    , link :: Text
    , hasReservation :: Bool
    }

asSqlBackendReader :: ReaderT SqlBackend m a -> ReaderT SqlBackend m a
asSqlBackendReader = Prelude.id

connStr = "host=localhost dbname=test user=test password=test port=5432"

runDb :: IO ()
runDb = do
  stateParks <- getStateParks
  -- updatedStateParksWithReservation <- updateStateParksWithReservation
  runStderrLoggingT $
    withPostgresqlPool connStr 10 $ \pool ->
      liftIO $ do
        flip runSqlPersistMPool pool $ do
          runMigration migrateAll
          repsertMany stateParks
  >> stopGlobalPool

getStateParks :: IO [(Key StatePark, StatePark)]
getStateParks = do
  scrapedFromURL <- scrapeURL stateParksURL stateParksScraper
  parallel (Prelude.map (\r -> getReservationStatus r) (Prelude.take 10 $ fromJust scrapedFromURL))

getReservationStatus :: ScrapedStatePark -> IO (Key StatePark, StatePark)
getReservationStatus (ScrapedStatePark n l False) = do
  linksOnPage <- scrapeURL (unpack l) reservationStatusScraper
  time <- getCurrentTime
  return $ (StateParkKey (slugify n) , StatePark (n) (slugify n) (l) (Prelude.elem ("http://www.reservecalifornia.com/" :: Text) (fromJust linksOnPage)) time)


-- URLs
stateParksURL :: String
stateParksURL = "https://www.parks.ca.gov/?page_id=21805"

-- Scrapers
reservationStatusScraper :: Scraper Text [Text]
reservationStatusScraper =
  attrs "href" "a"

stateParksScraper :: Scraper String [ScrapedStatePark]
stateParksScraper = chroot ("div" @: ["id" @= "center_content"]) (chroots ("li") (stateParkScraper))

stateParkScraper :: Scraper String ScrapedStatePark
stateParkScraper = do
  name <- text $ "a"
  link <- attr "href" $ "a"
  return $ ScrapedStatePark (pack name) (pack link) False

-- stateParkScraper' :: Scraper String ScrapedStatePark
-- stateParkScraper' = do
--   name <- text $ "a"
--   link <- attr "href"
--   return $ ScrapedStatePark name link False

-- Set Custom User Agent for HTTPS to work
managerSettings :: HTTP.ManagerSettings
managerSettings =
  HTTP.tlsManagerSettings
    { HTTP.managerModifyRequest =
        \req -> do
          req' <- HTTP.managerModifyRequest HTTP.tlsManagerSettings req
          return $
            req'
              { HTTP.requestHeaders =
                  (HTTP.hUserAgent, "My Custom UA") : HTTP.requestHeaders req'
              }
    }

backend :: Backend BackendRoute FrontendRoute
backend =
  Backend
    { _backend_run =
        \serve -> do
          runDb
          return ()
    , _backend_routeEncoder = fullRouteEncoder
    }

slugify :: Text -> Text
slugify s =
  let repl ' ' = '-'
      repl c = c
  in
    toLower $ (Data.Text.map repl (s))
