{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module Models.Biker where

import GPX.Track
import Control.Lens

data Biker = Biker { _bikerConstantDragNormalCoeff :: Double
                   , _bikerLinearDragNormalCoeff   :: Double
                   , _bikerQuadraticAeroDragCoeff  :: Double
                   , _bikerTotalWeight             :: Double} deriving Show

makeLenses ''Biker

g :: Double
g = 9.81

computeBikerPower :: Biker -> MechInfo -> Double
computeBikerPower Biker{..} MechInfo{..} =
  let normalForce   = cos _mechInfoInclinationAngle*_bikerTotalWeight*g                        in
  let frictionForce = _bikerConstantDragNormalCoeff * normalForce
                    +  _bikerLinearDragNormalCoeff * normalForce * _mechInfoSpeed
                    + _bikerQuadraticAeroDragCoeff * _mechInfoSpeed^2                          in
  let gravityDrag   = _bikerTotalWeight * sin _mechInfoInclinationAngle * g                    in
  let bikerForce    = frictionForce + gravityDrag + _mechInfoAcceleration * _bikerTotalWeight  in
  bikerForce * _mechInfoSpeed
