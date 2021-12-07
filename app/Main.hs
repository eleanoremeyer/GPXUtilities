module Main where

import Lib
import GPX.GPXParser
import GPX.Track
import System.Environment
import qualified Data.Vector as V
import qualified Data.List as L
import Data.Function (on)
import Models.Biker

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

-- https://www.leifiphysik.de/mechanik/reibung-und-fortbewegung/ausblick/reibungskraefte-beim-fahrradfahren
demoBiker :: Biker
demoBiker = Biker { _bikerTotalWeight = 80
                  , _bikerConstantDragNormalCoeff = 0.00349
                  , _bikerLinearDragNormalCoeff = 0
                  , _bikerQuadraticAeroDragCoeff = 0.189}

main :: IO ()
main = do
  filename <- head <$> getArgs
  Just track <- parseGpx filename
  print $ V.length track
  putStrLn $ "Total Distance: "<>show (totalDistance track)<>" m"
  let mechInfo = computeTrackMechInfo track
  putStrLn $ "topSpeed " <> show (V.maximumBy (compare `on` (_mechInfoSpeed . snd)) $ V.zip track mechInfo)

  let diag = [(tP^.trackPointTime,tP^.trackPointCoordinate.coordinateElevation,mI^.mechInfoSpeed,bP) |
                (tP,mI,bP)<-V.toList (V.zip3 track mechInfo (V.map (computeBikerPower demoBiker) mechInfo))]
  toFile def "example2_big.png" $ do
    layoutlr_left_axis . laxis_override .= axisGridHide
    layoutlr_right_axis . laxis_override .= axisGridHide
    -- plotLeft (line "Elevation" [ [ (t,e) | (t,e,_,_) <- diag] ])
    plotLeft (line "Power (W)" [ [ (t,bP) | (t,e,_,bP) <- diag] ])
    plotRight (line "Speed" [ [ (t,v*3.6) | (t,_,v,_) <- diag] ])
