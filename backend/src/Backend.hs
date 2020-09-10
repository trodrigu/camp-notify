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

-- Database Persistence
share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
StatePark
  name String
  hasReservation Bool
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
  stateParks <- produceStateParks
  runStderrLoggingT $
    withPostgresqlPool connStr 10 $ \pool ->
      liftIO $ do
        flip runSqlPersistMPool pool $ do
          runMigration migrateAll
          insertMany_ stateParks

produceStateParks :: IO [StatePark]
produceStateParks = do
  scrapedFromURL <- scrapeURL stateParksURL stateParksScraper
  return $
    Prelude.map scrapedMapConverter (Prelude.concat $ fromJust scrapedFromURL)
    -- TODO: sub out link and hasReservation in favor of actual values
  where
    scrapedMapConverter (ScrapedStatePark n "" True) = StatePark (unpack n) True

-- URLs
stateParksURL :: String
stateParksURL = "https://www.parks.ca.gov/?page_id=21805"

-- Scrapers
stateParksScraper :: Scraper String [[ScrapedStatePark]]
stateParksScraper = chroots ("ul" @: [hasClass "results-area"]) stateParkScraper

stateParkScraper :: Scraper String [ScrapedStatePark]
stateParkScraper = do
  names <- texts $ "a"
  -- array of strings to array of stateparks
  return $ Prelude.map (\r -> makeStatePark r) (Prelude.map pack names)
  where
    makeStatePark n = ScrapedStatePark n

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
