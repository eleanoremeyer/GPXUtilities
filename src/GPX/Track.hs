{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module GPX.Track where

import Data.Time
import Debug.Trace
import Control.Lens
import Data.Vector as V
import Text.XML

data Coordinate = Coordinate { _coordinateLatitude :: Double
                             , _coordinateLongitude :: Double
                             , _coordinateElevation :: Double } deriving Show

data TrackPoint = TrackPoint { _trackPointCoordinate :: Coordinate
                             , _trackPointTime :: UTCTime
                             , _trackPointExtensions :: [Element] } deriving Show

data MechDebugInfo = MechDebugInfo { debugTime :: Double, debugDist :: Double}
instance Show MechDebugInfo where
  show MechDebugInfo{..} = "speed="<>show debugDist<>"/"<>show debugTime

data MechInfo = MechInfo { _mechInfoSpeed :: Double
                         , _mechInfoAcceleration :: Double
                         , _mechInfoVertSpeed :: Double
                         , _mechInfoInclinationAngle :: Double
                         , debugField :: MechDebugInfo }

instance Show MechInfo where
  show MechInfo{..} = "Speed: "<>show _mechInfoSpeed<>" accel: "<>show _mechInfoAcceleration<>" debug: "<>show debugField<>"="<>show _mechInfoSpeed

makeLenses ''Coordinate
makeLenses ''TrackPoint
makeLenses ''MechInfo

type Track = Vector TrackPoint

distance :: Coordinate -> Coordinate -> Double
distance p q = sqrt (flatDistance p q^2 + (q^.coordinateElevation - p^.coordinateElevation)^2)

-- result in m, taken from https://rosettacode.org/wiki/Haversine_formula#Haskell
flatDistance p q = 1000 * distDeg 6371 (p^.coordinateLatitude,p^.coordinateLongitude) (q^.coordinateLatitude,q^.coordinateLongitude)
  where
    haversine = (^ 2) . sin . (/ 2)
    distDeg radius p1 p2 = distRad radius (deg2rad p1) (deg2rad p2)
    distRad radius (lat1, lng1) (lat2, lng2) =
      (2 * radius) * asin (min 1 ( sqrt $ haversine (lat2 - lat1) + ( (cos lat1 * cos lat2) * haversine (lng2 - lng1))))
    deg2rad = bimap d2r d2r
    d2r = (/ 180) . (pi *)

totalDistance :: Track -> Double
totalDistance v = V.sum $
  V.imap (\i p -> if i == 0 then 0 else distance (v!(i-1)^.trackPointCoordinate) (p^.trackPointCoordinate)) v

-- first tuple = last two entries, second tuple = next two entries
computeMechInfo :: (TrackPoint,TrackPoint) -> TrackPoint -> (TrackPoint, TrackPoint) -> MechInfo
computeMechInfo (prevprev,prev) curr (next,nextnext) =
  let distl = distance (prev^.trackPointCoordinate) (curr^.trackPointCoordinate) in
  let distr = distance (curr^.trackPointCoordinate) (next^.trackPointCoordinate) in
  let dist  = distl + distr in

  let timel = timeDiff curr prev in
  let timer = timeDiff next curr in
  let time = timeDiff next prev in

  let flatDist = flatDistance (prev^.trackPointCoordinate) (curr^.trackPointCoordinate)
               + flatDistance (curr^.trackPointCoordinate) (next^.trackPointCoordinate) in
  let flatSpeed = flatDist / (time + eps) in

  let speed  = dist  / (time + eps) in
  let speedl = distl / (timel+eps) in
  let speedr = distr / (timer+eps) in

  let verticalSpeed = sqrt (speed^2 - flatSpeed^2) in

  let accel = (speedr - speedl)/((time/2)+eps) :: Double in -- time/2 should be correct here since 2nd order
  MechInfo { _mechInfoSpeed = speed
           , _mechInfoAcceleration = accel
           , _mechInfoVertSpeed = verticalSpeed
           , _mechInfoInclinationAngle = atan (verticalSpeed / (speed+eps))
           , debugField = MechDebugInfo time dist }
  where
    eps :: Double
    eps = 0.001 -- Avoid div by zero
    timeDiff :: TrackPoint -> TrackPoint -> Double
    timeDiff p q = realToFrac $ nominalDiffTimeToSeconds $ diffUTCTime (p^.trackPointTime) (q^.trackPointTime)


computeTrackMechInfo :: Vector TrackPoint -> Vector MechInfo
computeTrackMechInfo v =
  let l = V.length v in
  let access i | i <  0     = v!0
               | i >= l     = v!(l-1)
               | otherwise  = v!i     in
  flip V.imap v $ \i p -> computeMechInfo (access (i-2), access (i-1)) p (access (i+1), access (i+2))

