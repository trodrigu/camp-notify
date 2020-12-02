{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE EmptyDataDecls #-}

module Main where

import Data.Aeson
import Control.Concurrent.ParallelIO
import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Reader
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as C
import Data.Maybe
import Data.Text
import Data.Time
import Data.Time.Clock
import Data.Map (Map, elems)
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH
import GHC.Generics
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP
import qualified Network.HTTP.Types.Header as HTTP
import Text.HTML.Scalpel
import Data.Text.Encoding (encodeUtf8)
import System.Random

-- DB schema
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
  reserveCaliforniaPlaceId Int
  latitude Double
  longitude Double
  deriving Show

Facility
  facilityName Text
  Primary facilityName
  facilityLatitude Double
  facilityLongitude Double
  facilityId Int
  stateParkNameSlug Text

Unit
  unitId Int
  Primary unitId
  unitName Text
  facilityId Int

Availability
  day Day
  unitId Int
  isFree Bool
  unitIdDay Text
  Primary unitIdDay
|]

asSqlBackendReader :: ReaderT SqlBackend m a -> ReaderT SqlBackend m a
asSqlBackendReader = Prelude.id

connStr = "host=localhost dbname=test user=test password=test port=5432"

-- Data transformation
getFacilitiesFromSelectedPlace :: (PlaceResponse, StatePark) -> [Facility]
getFacilitiesFromSelectedPlace (res, StatePark _ nameSlug _ _ _ _ _ _) =
  Prelude.map (
    \r -> fromFacilityRes r nameSlug)
    (elems $ fromJust $ facilityResFacilities $ selectedPlace res)

fromReserveCaliforniaStatePark :: StatePark -> ReserveCaliforniaStatePark -> StatePark
fromReserveCaliforniaStatePark (StatePark name nameSlug link hasReservation lastSync _ _ _) (ReserveCaliforniaStatePark _ _ lat long _ _ _ _ placeId) =
  StatePark name nameSlug link hasReservation lastSync placeId lat long


fromFacilityRes :: FacilityRes -> Text -> Facility
fromFacilityRes
  (FacilityRes
    facilityId
    facilityName
    description
    rateMessage
    facilityType
    facilityTypeNew
    inSeason
    facilityAvailable
    facilityAvailableFiltered
    facilityRestrictions
    latitude
    longitude
    category
    enableCheckOccupancy
    availableOccupancy
    unitTypes) =
      Facility
        (slugify facilityName)
        (withDefault latitude)
        (withDefault longitude)
        facilityId
        where
          withDefault n =
            case n of
              Just l -> l
              Nothing -> 0

fromFacilityGridResponseToUnits :: FacilityGridResponse -> [Unit]
fromFacilityGridResponseToUnits
  (FacilityGridResponse
    gridResponseFacilityId
    gridResponseName
    gridResponseDescription
    gridResponseFacilityType
    facilityMapSize
    facilityImage
    facilityImageVBT
    datesInSeason
    datesOutOfSeason
    seasonDates
    trafficStatuses
    unitCount
    availableUnitCount
    gridResponseSliceCount
    availableSliceCount
    gridResponseRestrictions
    units
  )
   =
     Prelude.map (
       \(UnitRes
          unitId
          unitName
          shortName
          recentPopups
          unitIsAda
          allowWebBooking
          mapInfo
          isWebViewable 
          isFiltered
          unitCategoryId
          sleepingUnitIds
          unitGroupId 
          unitTypeId
          vehicleLength
          orderBy
          sliceCount
          unitAvailableCount
          slices) ->
            Unit
              unitId
              (slugify unitName)
              gridResponseFacilityId
     )
     unitResponses
     where
       unitResponses = elems units

fromFacilityGridResponseToAvailabilities :: FacilityGridResponse -> [Availability]
fromFacilityGridResponseToAvailabilities
  (FacilityGridResponse
    gridResponseFacilityId
    gridResponseName
    gridResponseDescription
    gridResponseFacilityType
    facilityMapSize
    facilityImage
    facilityImageVBT
    datesInSeason
    datesOutOfSeason
    seasonDates
    trafficStatuses
    unitCount
    availableUnitCount
    gridResponseSliceCount
    availableSliceCount
    gridResponseRestrictions
    units
  )
   =
     Prelude.concatMap (
       \(UnitRes
          unitId
          unitName
          shortName
          recentPopups
          unitIsAda
          allowWebBooking
          mapInfo
          isWebViewable 
          isFiltered
          unitCategoryId
          sleepingUnitIds
          unitGroupId 
          unitTypeId
          vehicleLength
          orderBy
          sliceCount
          unitAvailableCount
          slices) ->
            Prelude.map (\slice -> 
              Availability 
                (date slice) 
                unitId
                (isFree slice)
                (append (pack $ show unitId) (pack $ show $ date slice)))
            (elems slices)
     )
     unitResponses
     where
       unitResponses = elems units

facilityTuplify :: [Facility] -> [(Key Facility, Facility)]
facilityTuplify facs =
  Prelude.map (
    \(Facility n lat long facId ns) -> 
      (FacilityKey $ slugify n, Facility n lat long facId ns)) 
      facs

unitTuplify :: [Unit] -> [(Key Unit, Unit)]
unitTuplify facs =
  Prelude.map (\(Unit id n facId) -> (UnitKey $ id, Unit id (slugify n) facId)) facs

availabilityTuplify :: [Availability] -> [(Key Availability, Availability)]
availabilityTuplify avails =
  Prelude.map (
    \a -> (AvailabilityKey 
            $ append 
                (pack $ show $ availabilityUnitId a) 
                (pack $ show $ availabilityDay a)
          , a)) avails

toGridRequest :: (PlaceResponse, StatePark) -> Day -> [GridRequest]
toGridRequest res day =
  let facilities = getFacilitiesFromSelectedPlace res
  in
  Prelude.map (\r -> fromFacilityToGridRequest r day) facilities

fromFacilityToGridRequest :: Facility -> Day -> GridRequest
fromFacilityToGridRequest (Facility
                            facilityName
                            latitude
                            longitude
                            facilityId
                            nameSlug) 
                            day =
  GridRequest
    facilityId
    0
    day
    True
    True
    False
    0
    0
    0
    []
    day
    (fromJust $ fromGregorianValid 2021 10 12)

defaultGridResponse :: GridResponse
defaultGridResponse = error "not implemented"


fromFacilityGridResponse :: StatePark -> FacilityGridResponse -> StatePark
fromFacilityGridResponse = error "not implemented"


-- IO like http requests
getPlaceResponses :: [(Key StatePark, StatePark)] -> IO [(PlaceResponse, StatePark)]
getPlaceResponses sps = do
  time <- getCurrentTime
  let day = utctDay time
  parallel $ Prelude.map (\(_, StatePark name nameSlug link hasReservation lastSync placeId lat long) -> do
      let sp = StatePark name nameSlug link hasReservation lastSync placeId lat long
      postRes <- postPlace makeFacilitiesURL sp day
      let decodedRes = fromJust $ decode $ postRes
      return (decodedRes, sp)
    )
    sps


getGridResponses :: [GridRequest] -> IO [GridResponse]
getGridResponses grds = do
  time <- getCurrentTime
  let day = utctDay time
  parallel $ Prelude.map (
    \grd
    -> do
      getGrids <- postGrid makeGridURL grd day
      let gs = decode $ getGrids
      case gs of
        Just g -> return g
        Nothing -> do
          writeJsonToFile "./gridres" (show getGrids)
          return defaultGridResponse
    )
    grds

getStateParks :: IO [(Key StatePark, StatePark)]
getStateParks = do
  scrapedFromURL <- scrapeURL stateParksURL stateParksScraper
  parallel (Prelude.map (\r -> getReservationStatus r) (fromJust scrapedFromURL))

getReservationStatus :: ScrapedStatePark -> IO (Key StatePark, StatePark)
getReservationStatus (ScrapedStatePark n l False) = do
  linksOnPage <- scrapeURL (unpack l) reservationStatusScraper
  time <- getCurrentTime
  return $ (StateParkKey (slugify n) , StatePark (Data.Text.replace "State Park" "SP" n) (slugify n) (l) (Prelude.elem ("http://www.reservecalifornia.com/" :: Text) (fromJust linksOnPage)) time 0 0.0 0.0)

postPlace :: String -> StatePark -> Day -> IO B.ByteString
postPlace jsonURL (StatePark name nameSlug link hasReservation lastSync placeId lat long) day = do
  initRequest <- HTTP.parseRequest jsonURL
  manager <- HTTP.newManager managerSettings
  let placeIdAsString = show placeId :: String
  let requestObject = PlaceRequest { placeRequestPlaceId = (pack placeIdAsString)
                                   ,  placeRequestLatitude = lat
                                   ,  placeRequestLongitude = long
                                   ,  highlightedPlaceId = 0
                                   ,  startDate = day
                                   ,  nights = pack "1"
                                   ,  countNearby = True
                                   ,  nearbyLimit = 100
                                   ,  nearbyOnlyAvailable = False
                                   ,  nearbyCountLimit = 10
                                   ,  sort = "Distance"
                                   ,  customerId = pack "0"
                                   ,  refreshFavourites = True
                                   ,  isADA = False
                                   ,  placeRequestUnitCategoryId = 0
                                   ,  placeRequestSleepingUnitId = 0
                                   ,  placeRequestMinVehicleLength = 0
                                   ,  unitTypesGroupIds = []
  }
  let reqWithBody = initRequest { HTTP.requestBody = HTTP.RequestBodyLBS $ encode requestObject }
  let reqWithHeaders = reqWithBody { HTTP.requestHeaders = [(HTTP.hContentType, "application/json")]}
  res <- HTTP.httpLbs reqWithHeaders manager
  return $ HTTP.responseBody res

postGrid :: String -> GridRequest -> Day -> IO B.ByteString
postGrid jsonURL gridRequest day = do
  initRequest <- HTTP.parseRequest jsonURL
  manager <- HTTP.newManager managerSettings
  let reqWithBody = initRequest { HTTP.requestBody = HTTP.RequestBodyLBS $ encode gridRequest }
  let reqWithHeaders = reqWithBody { HTTP.requestHeaders = [(HTTP.hContentType, "application/json")]}
  res <- HTTP.httpLbs reqWithHeaders manager
  return $ HTTP.responseBody res

getJSON :: String -> IO B.ByteString
getJSON jsonURL = do
  request <- HTTP.parseRequest jsonURL
  manager <- HTTP.newManager managerSettings
  res <- HTTP.httpLbs request manager
  return $ HTTP.responseBody res

getStateParkPlaces :: [(Key StatePark, StatePark)] -> IO [(Key StatePark, StatePark)]
getStateParkPlaces sps = do
  parallel $ Prelude.map (\(_, sp) -> do
      let (StatePark n _ _ _ _ _ _ _) = sp
      ds <- (eitherDecode <$> getJSON (makePlaceURL n)) :: IO (Either String [ReserveCaliforniaStatePark])
      case ds of
        Left err -> return $ (StateParkKey $ slugify n, sp)
        Right reserveStateParks -> do
          case reserveStateParks of
            [] -> 
              return (StateParkKey $ slugify n, sp)
            parks -> do
              let rp:rest = parks
              return (StateParkKey $ slugify n, fromReserveCaliforniaStatePark sp rp)
    )
    sps


-- Seed DB
runDb :: IO ()
runDb = do
  stateParks <- getStateParks

  let parksWithReservation = 
        Prelude.filter (
          \(_, StatePark _ _ _ hasReservation _ _ _ _) -> hasReservation) stateParks

  let parksWithoutReservation =
        Prelude.filter (
          \(_, StatePark _ _ _ hasReservation _ _ _ _) -> not hasReservation) stateParks

  let parksWithSPSuffix = 
        Prelude.filter (
          \(_, StatePark n _ _ _ _ _ _ _) -> Data.Text.isSuffixOf "SP" n) parksWithReservation

  -- TODO: Not sure if these are important to capture
  let parksWithoutSpSuffix =
        Prelude.filter (
          \(_, StatePark n _ _ _ _ _ _ _) -> 
            not $ Data.Text.isSuffixOf "SP" n) parksWithReservation

  stateParksWithPlaceIds <- getStateParkPlaces parksWithSPSuffix

  placeResponses <- getPlaceResponses stateParksWithPlaceIds

  time <- getCurrentTime
  let day = utctDay time

  let facilitiesForDb = Prelude.concatMap (\r -> getFacilitiesFromSelectedPlace r) placeResponses

  let gridRequests = Prelude.concatMap (\r -> toGridRequest r day) placeResponses
  gridRes <- getGridResponses gridRequests

  let unitsForDb = 
        Prelude.concatMap (\r -> fromFacilityGridResponseToUnits $ facility r) gridRes
  let availabilitiesForDb =
        Prelude.concatMap (\r -> fromFacilityGridResponseToAvailabilities $ facility r) gridRes

  runStderrLoggingT $
    withPostgresqlPool connStr 10 $ \pool ->
      liftIO $ do
        flip runSqlPersistMPool pool $ do
          runMigration migrateAll
          repsertMany $ stateParksWithPlaceIds
          repsertMany $ unitTuplify unitsForDb
          repsertMany $ facilityTuplify facilitiesForDb
          repsertMany $ availabilityTuplify availabilitiesForDb

  >> stopGlobalPool

main :: IO ()
main = putStrLn "Syncing DB..." >> runDb




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

slugify :: Text -> Text
slugify s =
  let repl ' ' = '-'
      repl c = c
  in
    toLower $ (Data.Text.map repl (s))


-- Data or Model layer
data ScrapedStatePark =
  ScrapedStatePark
    { name :: Text
    , link :: Text
    , hasReservation :: Bool
    }

data Restrictions = Restrictions
  { futureBookingStarts :: UTCTime
  , futureBookingEnds :: UTCTime
  , minimumStay :: Int
  , maximumStay :: Int 
  , isRestrictionValid :: Bool
  } deriving (Generic, Show)

instance FromJSON Restrictions where
  parseJSON = withObject "Restrictions" $ \o -> 
    Restrictions 
      <$> o.: "FutureBookingStarts"
      <*> o .:  "FutureBookingEnds"
      <*> o .:  "MinimumStay"
      <*> o .:  "MaximumStay"
      <*> o .:  "IsRestrictionValid"

data UnitType = UnitType
  { id :: Int -- unitTypeId
  , unitTypeName :: Text -- name
  , available :: Bool
  , availableFiltered :: Bool
  , unitTypeCategoryId :: Int -- unitCategoryId
  , unitTypeGroupId :: Int
  , maxVehicleLength :: Int
  , hasAda :: Bool
  , restrictions :: Maybe Restrictions
  , availableCount :: Int
  } deriving (Show, Generic)

instance FromJSON UnitType where
  parseJSON = withObject "UnitType" $ \o -> 
    UnitType
      <$> o.: "UnitTypeId"
      <*> o.: "Name"
      <*> o.: "Available"
      <*> o.: "AvailableFiltered"
      <*> o.: "UnitCategoryId"
      <*> o.: "UnitTypeGroupId"
      <*> o.: "MaxVehicleLength"
      <*> o.: "HasAda"
      <*> o.: "Restrictions"
      <*> o.: "AvailableCount"

data FacilityRes = FacilityRes
  { facilityId :: Int
  , facilityName :: Text
  , description :: Maybe Text
  , rateMessage :: Maybe Text
  , facilityType :: Int
  , facilityTypeNew :: Int
  , inSeason :: Bool
  , facilityAvailable :: Bool
  , facilityAvailableFiltered :: Bool
  , facilityRestrictions :: Maybe Restrictions
  , latitude :: Maybe Double
  , longitude :: Maybe Double
  , category :: Maybe Text
  , enableCheckOccupancy :: Bool
  , availableOccupancy :: Maybe Int
  , unitTypes :: Map Text UnitType
  } deriving (Show, Generic)

instance FromJSON FacilityRes where
  parseJSON = withObject "FacilityRes" $ \o -> 
    FacilityRes
      <$> o.:  "FacilityId"
      <*> o.:  "Name"
      <*> o.:  "Description"
      <*> o.:  "RateMessage"
      <*> o.:  "FacilityType"
      <*> o.:  "FacilityTypeNew"
      <*> o.:  "InSeason"
      <*> o.:  "Available"
      <*> o.:  "AvailableFiltered"
      <*> o.:  "Restrictions"
      <*> o.:  "Latitude"
      <*> o.:  "Longitude"
      <*> o.:  "Category"
      <*> o.:  "EnableCheckOccupancy"
      <*> o.:  "AvailableOccupancy"
      <*> o.:  "UnitTypes"

data FacilityGridResponse = FacilityGridResponse
  { gridResponseFacilityId :: Int -- facilityId
  , gridResponseName :: Text -- name
  , gridResponseDescription :: Text -- description
  , gridResponseFacilityType :: Int -- facilityType
  , facilityMapSize :: Bool
  , facilityImage :: Text
  , facilityImageVBT :: Text
  , datesInSeason :: Int
  , datesOutOfSeason :: Int
  , seasonDates :: Map Text Bool
  , trafficStatuses :: Map Text Text
  , unitCount :: Int
  , availableUnitCount :: Int
  , gridResponseSliceCount :: Int -- sliceCount
  , availableSliceCount :: Int
  , gridResponseRestrictions :: Restrictions -- restrictions
  , units :: Map Text UnitRes
  } deriving (Generic, Show)

instance FromJSON FacilityGridResponse where
  parseJSON = withObject "FacilityGridResponse" $ \o -> 
    FacilityGridResponse 
      <$> o .:  "FacilityId"
      <*> o .:  "Name"
      <*> o .:  "Description"
      <*> o .:  "FacilityType"
      <*> o .:  "FacilityMapSize"
      <*> o .:  "FacilityImage"
      <*> o .:  "FacilityImageVBT"
      <*> o .:  "DatesInSeason"
      <*> o .:  "DatesOutOfSeason"
      <*> o .:  "SeasonDates"
      <*> o .:  "TrafficStatuses"
      <*> o .:  "UnitCount"
      <*> o .:  "AvailableUnitCount"
      <*> o .:  "SliceCount"
      <*> o .:  "AvailableSliceCount"
      <*> o .:  "Restrictions"
      <*> o .:  "Units"


data Filters = Filters
  { isAda :: Text
  , filtersUnitCategoryId :: Text -- UnitCategoryId
  , sleepingUnitId :: Text
  , minVehicleLength :: Text } deriving (Generic, Show)

instance FromJSON Filters where
  parseJSON = withObject "Filters" $ \o -> 
    Filters
      <$> o.: "IsADA"
      <*> o.: "UnitCategoryId"
      <*> o.: "SleepingUnitId"
      <*> o.: "MinVehicleLength"

data Place = Place
  { placeId :: Int
  , placeName :: Text -- name
  , placeDescription :: Text -- description
  , hasAlerts :: Bool
  , isFavourite :: Bool
  , allhighlights :: Text
  , url :: Text
  , imageUrl :: Text
  , bannerUrl :: Maybe Text
  , parkSize :: Text
  , placeLatitude :: Double -- latitude
  , placeLongitude :: Double -- longitude
  , milesFromSelected :: Int
  , placeAvailable :: Bool
  , placeAvailableFiltered :: Bool -- availableFiltered
  , parkCategoryId :: Int
  , parkActivity :: Int
  , parkPopularity :: Int
  , placeAvailableUnitCount :: Int -- availableUnitCount
  , placeRestrictions :: Restrictions -- restrictions
  , facilityResFacilities :: Maybe (Map Text FacilityRes)
  } deriving (Show, Generic)

instance FromJSON Place where
  parseJSON = withObject "Place" $ \o -> 
    Place
      <$> o.: "PlaceId"
      <*> o.: "Name"
      <*> o.: "Description"
      <*> o.: "HasAlerts"
      <*> o.: "IsFavourite"
      <*> o.: "Allhighlights"
      <*> o.: "Url"
      <*> o.: "ImageUrl"
      <*> o.: "BannerUrl"
      <*> o.: "ParkSize"
      <*> o.: "Latitude"
      <*> o.: "Longitude"
      <*> o.: "MilesFromSelected"
      <*> o.: "Available"
      <*> o.: "AvailableFiltered"
      <*> o.: "ParkCategoryId"
      <*> o.: "ParkActivity"
      <*> o.: "ParkPopularity"
      <*> o.: "AvailableUnitCount"
      <*> o.: "Restrictions"
      <*> o.: "Facilities"

data PlaceRequest = PlaceRequest
  { placeRequestPlaceId :: Text
  , placeRequestLatitude :: Double
  , placeRequestLongitude :: Double
  , highlightedPlaceId :: Int
  , startDate :: Day
  , nights :: Text
  , countNearby :: Bool
  , nearbyLimit :: Int
  , nearbyOnlyAvailable :: Bool
  , nearbyCountLimit :: Int
  , sort :: Text
  , customerId :: Text
  , refreshFavourites :: Bool
  , isADA :: Bool
  , placeRequestUnitCategoryId :: Int
  , placeRequestSleepingUnitId :: Int
  , placeRequestMinVehicleLength :: Int
  , unitTypesGroupIds :: [Int]
  } deriving (Show, Generic)
instance ToJSON PlaceRequest where
    toJSON (PlaceRequest 
              placeId 
              placeRequestLatitude 
              placeRequestLongitude 
              highlightedPlaceId 
              startDate 
              nights 
              countNearby 
              nearbyLimit 
              nearbyOnlyAvailable 
              nearbyCountLimit 
              sort 
              customerId 
              refreshFavourites 
              isADA 
              placeRequestUnitCategoryId 
              placeRequestSleepingUnitId 
              placeRequestMinVehicleLength 
              unitTypesGroupIds) = object [ "PlaceId" .= placeId 
                                          , "Latitude" .= placeRequestLatitude 
                                          , "Longitude" .= placeRequestLongitude 
                                          , "HighlightedPlaceId" .= highlightedPlaceId 
                                          , "StartDate" .= startDate 
                                          , "Nights" .= nights 
                                          , "CountNearby" .= countNearby 
                                          , "NearbyLimit" .= nearbyLimit 
                                          , "NearbyOnlyAvailable" .= nearbyOnlyAvailable 
                                          , "NearbyCountLimit" .= nearbyCountLimit 
                                          , "Sort" .= sort 
                                          , "CustomerId" .= customerId 
                                          , "RefreshFavourites" .= refreshFavourites 
                                          , "IsADA" .= isADA 
                                          , "UnitCategoryId" .= placeRequestUnitCategoryId 
                                          , "SleepingUnitId" .= placeRequestSleepingUnitId 
                                          , "MinVehicleLength" .= placeRequestMinVehicleLength 
                                          , "UnitTypesGroupIds" .= unitTypesGroupIds ]

    toEncoding (PlaceRequest 
              placeRequestPlaceId 
              placeRequestLatitude 
              placeRequestLongitude 
              highlightedPlaceId 
              startDate 
              nights 
              countNearby 
              nearbyLimit 
              nearbyOnlyAvailable 
              nearbyCountLimit 
              sort 
              customerId 
              refreshFavourites 
              isADA 
              placeRequestUnitCategoryId 
              placeRequestSleepingUnitId 
              placeRequestMinVehicleLength 
              unitTypesGroupIds) = 
              pairs ("PlaceId" .= placeRequestPlaceId 
                      <> "Latitude" .= placeRequestLatitude 
                      <> "Longitude" .= placeRequestLongitude 
                      <> "HighlightedPlaceId" .= highlightedPlaceId 
                      <> "StartDate" .= startDate 
                      <> "Nights" .= nights 
                      <> "CountNearby" .= countNearby 
                      <> "NearbyLimit" .= nearbyLimit 
                      <> "NearbyOnlyAvailable" .= nearbyOnlyAvailable 
                      <> "NearbyCountLimit" .= nearbyCountLimit 
                      <> "Sort" .= sort 
                      <> "CustomerId" .= customerId 
                      <> "RefreshFavourites" .= refreshFavourites 
                      <> "IsADA" .= isADA 
                      <> "UnitCategoryId" .= placeRequestUnitCategoryId 
                      <> "SleepingUnitId" .= placeRequestSleepingUnitId 
                      <> "MinVehicleLength" .= placeRequestMinVehicleLength 
                      <> "UnitTypesGroupIds" .= unitTypesGroupIds)

data PlaceResponse = PlaceResponse
  { message :: Text
  , selectedPlaceId :: Int
  , placeResponseHighlightedPlaceId :: Int
  , placeResponseLatitude :: Double -- latitude
  , placeResponselongitude :: Double -- longitude
  , placeResponseStartDate :: Day
  , endDate :: Day
  , placeResponseCountNearby :: Bool
  , placeResponseNearbyLimit :: Int
  , placeResponseSort :: Text
  , placeResponseCustomerId :: Maybe Int
  , filters :: Filters
  , availablePlaces :: Int
  , selectedPlace :: Place
  , nearbyPlaces :: [Place]
  } deriving (Generic, Show)
instance FromJSON PlaceResponse where
  parseJSON = withObject "PlaceResponse" $ \o -> 
    PlaceResponse 
      <$> o.:  "Message"
      <*> o.:  "SelectedPlaceId"
      <*> o.:  "HighlightedPlaceId"
      <*> o.:  "Latitude"
      <*> o.:  "Longitude"
      <*> o.:  "StartDate"
      <*> o.:  "EndDate"
      <*> o.:  "CountNearby"
      <*> o.:  "NearbyLimit"
      <*> o.:  "Sort"
      <*> o.:  "CustomerId"
      <*> o.:  "Filters"
      <*> o.:  "AvailablePlaces"
      <*> o.:  "SelectedPlace"
      <*> o.:  "NearbyPlaces"

data GridRequest = GridRequest
  { gridRequestFacilityId  :: Int -- facilityId
  , gridRequestUnitTypeId :: Int -- unitTypeId
  , gridRequestStartDate :: Day -- startDate
  , inSeasonOnly :: Bool
  , webOnly :: Bool
  , gridRequestIsADA :: Bool -- isADA
  , gridRequestSleepingUnitId :: Int -- sleepingUnitId
  , gridRequestMinVehicleLength :: Int -- minVehicleLength
  , gridRequestUnitCategoryId :: Int -- unitCategoryId
  , gridRequestUnitTypesGroupIds :: [Int]
  , minDate :: Day
  , maxDate :: Day
  } deriving (Generic, Show)
instance ToJSON GridRequest where
    toJSON (GridRequest 
              gridRequestFacilityId  
              gridRequestUnitTypeId 
              gridRequestStartDate 
              inSeasonOnly
              webOnly
              gridRequestIsADA 
              gridRequestSleepingUnitId 
              gridRequestMinVehicleLength 
              gridRequestUnitCategoryId 
              gridRequestUnitTypesGroupIds
              minDate
              maxDate) =
        object ["FacilityId" .= gridRequestFacilityId  
               , "UnitTypeId" .= gridRequestUnitTypeId 
               , "StartDate" .= gridRequestStartDate 
               , "inSeasonOnly" .= inSeasonOnly
               , "webOnly" .= webOnly
               , "IsADA" .= gridRequestIsADA 
               , "SleepingUnitId" .= gridRequestSleepingUnitId 
               , "MinVehicleLength" .= gridRequestMinVehicleLength 
               , "UnitCategoryId" .= gridRequestUnitCategoryId 
               , "UnitTypesGroupIds" .= gridRequestUnitTypesGroupIds
               , "minDate" .= minDate
               , "maxDate" .= maxDate
               ]

    toEncoding (GridRequest 
              gridRequestFacilityId  
              gridRequestUnitTypeId 
              gridRequestStartDate 
              inSeasonOnly
              webOnly
              gridRequestIsADA 
              gridRequestSleepingUnitId 
              gridRequestMinVehicleLength 
              gridRequestUnitCategoryId 
              gridRequestUnitTypesGroupIds
              minDate
              maxDate) =
        pairs ("FacilityId" .= gridRequestFacilityId  
               <> "UnitTypeId" .= gridRequestUnitTypeId 
               <> "StartDate" .= gridRequestStartDate 
               <> "inSeasonOnly" .= inSeasonOnly
               <> "webOnly" .= webOnly
               <> "IsADA" .= gridRequestIsADA 
               <> "SleepingUnitId" .= gridRequestSleepingUnitId 
               <> "MinVehicleLength" .= gridRequestMinVehicleLength 
               <> "UnitCategoryId" .= gridRequestUnitCategoryId 
               <> "UnitTypesGroupIds" .= gridRequestUnitTypesGroupIds
               <> "minDate" .= minDate
               <> "maxDate" .= maxDate)

data ReserveCaliforniaStatePark = ReserveCaliforniaStatePark
  { cityParkId :: Int
  , reserveName :: Text
  , reserveStateParkLatitude :: Double -- latitude
  , reserveStateParkLongitude :: Double -- longitude
  , isActive :: Bool
  , entityType :: Text
  , enterpriseId :: Int
  , stateParkSize :: Text -- parkSize
  , stateParkPlaceId :: Int -- placeId
  } deriving (Generic, Show)

data Slice = Slice
  {
    date :: Day,
    isFree :: Bool,
    isBlocked :: Bool,
    isWalkin :: Bool,
    reservationId :: Int,
    lock :: Maybe Text,
    minStay :: Int
  } deriving (Generic, Show)

instance FromJSON Slice where
  parseJSON = withObject "Slice" $ \o -> 
    Slice 
      <$> o.: "Date"
      <*> o.: "IsFree"
      <*> o.: "IsBlocked"
      <*> o.: "IsWalkin"
      <*> o.: "ReservationId"
      <*> o.: "Lock"
      <*> o.: "MinStay"

data MapInfo = MapInfo
 { mapInfoUnitImage :: Text -- unitImage
 , mapInfoUnitImageVBT :: Text -- unitImageVBT
 , mapInfoImageCoordinateX :: Int -- imageCoordinateX
 , mapInfoImageCoordinateY :: Int -- imageCoordinateY
 , mapInfoImageWidth :: Int -- imageWidth
 , mapInfoImageHeight :: Int -- imageHeight
 , mapInfoFontSize :: Int -- fontSize
 } deriving (Generic, Show)

instance FromJSON MapInfo where
  parseJSON = withObject "MapInfo" $ \o -> 
    MapInfo 
      <$> o.:  "UnitImage"
      <*> o.:  "UnitImageVBT"
      <*> o.:  "ImageCoordinateX"
      <*> o.:  "ImageCoordinateY"
      <*> o.:  "ImageWidth"
      <*> o.:  "ImageHeight"
      <*> o.:  "FontSize"

data UnitRes = UnitRes
  { unitId :: Int
  , unitName :: Text -- name
  , shortName :: Text
  , recentPopups :: Int
  , unitIsAda :: Bool -- isAda
  , allowWebBooking :: Bool
  , mapInfo :: MapInfo
  , isWebViewable  :: Bool
  , isFiltered :: Bool
  , unitCategoryId :: Int
  , sleepingUnitIds :: Maybe [Int]
  , unitGroupId  :: Int -- unitTypeGroupId
  , unitTypeId :: Int
  , vehicleLength :: Int
  , orderBy :: Int
  , sliceCount :: Int
  , unitAvailableCount :: Int -- availableCount
  , slices :: Map Text Slice
  } deriving (Generic, Show)

instance FromJSON UnitRes where
  parseJSON = withObject "UnitRes" $ \o -> 
    UnitRes 
      <$> o.: "UnitId"
      <*> o.: "Name"
      <*> o.: "ShortName"
      <*> o.: "RecentPopups"
      <*> o.: "IsAda"
      <*> o.: "AllowWebBooking"
      <*> o.: "MapInfo"
      <*> o.: "IsWebViewable"
      <*> o.: "IsFiltered"
      <*> o.: "UnitCategoryId"
      <*> o.: "SleepingUnitIds"
      <*> o.: "UnitTypeGroupId"
      <*> o.: "UnitTypeId"
      <*> o.: "VehicleLength"
      <*> o.: "OrderBy"
      <*> o.: "SliceCount"
      <*> o.: "AvailableCount"
      <*> o.: "Slices"


data GridResponse = GridResponse
  { gridResponseMessage :: Text -- message
  , gridResponseFilters :: Filters -- filters
  , gridResponseUnitTypeId :: Int -- unitTypeId
  , gridResponseStartDate :: Day -- startDate
  , gridResponseEndDate :: Day -- endDate
  , todayDate :: Day
  , gridResponseMinDate :: Day -- minDate
  , gridResponseMaxDate :: Day -- maxDate
  , availableUnitsOnly :: Bool
  , unitSort :: Text
  , facility :: FacilityGridResponse
  } deriving (Generic, Show)
instance FromJSON GridResponse where
  parseJSON = withObject "GridResponse" $ \o -> 
    GridResponse
      <$> o .: "Message"  -- message
      <*> o.: "Filters"
      <*> o.: "UnitTypeId"
      <*> o.: "StartDate"
      <*> o.: "EndDate"
      <*> o.: "TodayDate"
      <*> o.: "MinDate"
      <*> o.: "MaxDate"
      <*> o.: "AvailableUnitsOnly"
      <*> o.: "UnitSort"
      <*> o.: "Facility"

instance ToJSON ReserveCaliforniaStatePark where
    toJSON (ReserveCaliforniaStatePark cityParkId name latitude longitude isActive entityType enterpriseId parkSize placeId) =
        object [ "CityParkId" .= cityParkId
               , "Name" .= name
               , "Latitude" .= latitude
               , "Longitude" .= longitude
               , "IsActive" .= isActive
               , "EntityType" .= entityType
               , "EnterpriseId" .= entityType
               , "ParkSize" .= parkSize
               , "PlaceId" .= placeId ]

    toEncoding (ReserveCaliforniaStatePark cityParkId name latitude longitude isActive entityType enterpriseId parkSize placeId) =
        pairs ("CityParkId" .= cityParkId 
              <> "Name" .= name
              <> "Latitude" .= latitude
              <> "Longitude" .= longitude
              <> "IsActive" .= isActive
              <> "EntityType" .= entityType
              <> "EnterpriseId" .= enterpriseId
              <> "ParkSize" .= parkSize
              <> "PlaceId" .= placeId)

instance FromJSON ReserveCaliforniaStatePark where
  parseJSON = withObject "ReserveCaliforniaStatePark" $ \o -> 
    ReserveCaliforniaStatePark 
      <$> o .: "CityParkId" 
      <*> o .: "Name" 
      <*> o .: "Latitude" 
      <*> o .: "Longitude"
      <*> o .: "IsActive"
      <*> o .: "EntityType"
      <*> o .: "EnterpriseId"
      <*> o .: "ParkSize"
      <*> o .: "PlaceId" 


-- URLs
stateParksURL :: String
stateParksURL = "https://www.parks.ca.gov/?page_id=21805"

makePlaceURL :: Text -> String
makePlaceURL pathWithParams = "GET https://calirdr.usedirect.com/rdr/rdr/fd/citypark/namecontains/" ++ unpack (Data.Text.replace " " "%20" pathWithParams) 

makeFacilitiesURL :: String
makeFacilitiesURL = "POST https://calirdr.usedirect.com/rdr/rdr/search/place"

makeGridURL :: String
makeGridURL = "POST https://calirdr.usedirect.com/rdr/rdr/search/grid"


-- Helper functions for debugging json
readInJson :: String -> IO B.ByteString
readInJson path = do
  f <- readFile path
  let s = B.fromStrict $ encodeUtf8 $ pack f
  return s

writeJsonToFile :: String -> String -> IO ()
writeJsonToFile name json = do
  g <- newStdGen
  let (i, _) = randomR (1 :: Int, 1000) g
  writeFile (name ++ show i ++ ".json") json
