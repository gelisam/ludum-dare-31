module Popup where

import Graphics.Gloss

import Animation


rotateAway :: Float -> Picture -> Picture
rotateAway t = translate 0 (-offset)
             . rotate angle
             . translate 0 offset
             . scale factor factor
  where
    offset = 960
    awayAngle = (-45)
    awayFactor = 1.5
    angle = animatedValue awayAngle (interpolate 1 0 awayAngle) t
    factor = animatedValue awayFactor (interpolate 1 1 awayFactor) t
