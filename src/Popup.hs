module Popup where

import Control.Applicative
import Graphics.Gloss

import Animation


-- -1 <= t <= 1
rotatePopup :: Float -> Picture -> Picture
rotatePopup t = translate 0 (-offset)
             . rotate angle
             . translate 0 offset
             . scale factor factor
  where
    offset = 960
    awayAngle = (-45)
    awayFactor = 1.5
    angle = animationValue awayAngle (interpolate 1 0 awayAngle) t
    factor = animationValue awayFactor (interpolate 1 1 awayFactor) t

rotateDuration :: Float
rotateDuration = 0.75

rotateIntoView :: Picture -> Animation Picture
rotateIntoView p = rotatePopup <$> interpolate rotateDuration (-1) 0
                               <*> pure p

rotateAway :: Picture -> Animation Picture
rotateAway p = rotatePopup <$> interpolate rotateDuration 0 1
                           <*> pure p
