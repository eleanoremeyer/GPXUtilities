{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module GPX.GPXParser where

import Text.XML as X
import Text.XML.Lens as X
import Data.Vector as V
import GPX.Track
import Data.Maybe (listToMaybe)
import Control.Monad
import Data.Text as T
import Control.Lens
import Debug.Trace
import Data.Map as M
import Data.Time

elementToTrackPoint :: Element -> Maybe TrackPoint
elementToTrackPoint e@Element{..} = do
  lat <- readText <$> M.lookup "lat" elementAttributes
  lon <- readText <$> M.lookup "lon" elementAttributes
  ele <- readText <$> e ^? named "trkpt" ... named "ele" . text
  time <- e ^? named "trkpt" ... named "time" . text >>= parseTimeM True defaultTimeLocale timeFormat . unpack
  let exts = e ^.. named "trkpt" ... named "extensions"
  pure $ TrackPoint { _trackPointCoordinate = Coordinate { _coordinateLatitude = lat
                                                         , _coordinateLongitude = lon
                                                         , _coordinateElevation = ele}
                    , _trackPointTime = time
                    , _trackPointExtensions = exts}
  where
    timeFormat = "%Y-%m-%dT%H:%M:%SZ"
    readText = read . unpack


filterElemNodes :: Element -> [Element]
filterElemNodes x = [e | NodeElement e <- elementNodes x]

parseGpx :: FilePath -> IO (Maybe Track)
parseGpx f = do
  X.readFile def f <&> \doc -> do
    trk <- doc ^? root . named "gpx" ... named "trk"
    -- trkName <- trk ^? named "trk" ... named "name" . text
    let trkPoints = trk ^.. named "trk" ... named "trkseg" ... named "trkpt"
    V.fromList <$> traverse elementToTrackPoint trkPoints

