module Popup where

import Data.Monoid
import Control.Applicative
import Graphics.Gloss
import Text.Printf

import Animation


fadeDuration :: Float
fadeDuration = 0.75

whiteFadeOutDuration :: Float
whiteFadeOutDuration = 2


fadeInAnimation :: Animation Float
fadeInAnimation = interpolate fadeDuration 0 1

fadeOutAnimation :: Animation Float
fadeOutAnimation = interpolate whiteFadeOutDuration 1 0


-- level popup (fade in, fade out)

staticLevelPopup :: Int -> Picture
staticLevelPopup n = staticWhiteFilter <> levelTitle n

prevLevelPopupAnimation :: Int -> Animation Picture
prevLevelPopupAnimation n = mappend <$> whitePopupAnimation
                                    <*> prevLevelTitleAnimation n

nextLevelPopupAnimation :: Int -> Animation Picture
nextLevelPopupAnimation n = mappend <$> whitePopupAnimation
                                    <*> nextLevelTitleAnimation n


-- white filter

staticWhiteFilter :: Picture
staticWhiteFilter = makeWhiteFilter 1

makeWhiteFilter :: Float -> Picture
makeWhiteFilter alpha = color white' (rectangleSolid 640 480)
  where
    white' :: Color
    white' = makeColor 1 1 1 (0.9 * alpha)

whiteFadeInAnimation :: Animation Picture
whiteFadeInAnimation = makeWhiteFilter <$> fadeInAnimation

whiteFadeOutAnimation :: Animation Picture
whiteFadeOutAnimation = makeWhiteFilter <$> fadeOutAnimation

whitePopupAnimation :: Animation Picture
whitePopupAnimation = whiteFadeInAnimation <> whiteFadeOutAnimation


-- text

levelTitle :: Int -> Picture
levelTitle = translate (-125) 100
           . scale 0.5 0.5
           . text
           . printf "Level %d"

levelTitleAnimation :: Int -> Animation Picture
levelTitleAnimation n = decelerate (rotateIntoView title)
                     <> accelerate (rotateAway title)
  where
    title = levelTitle n

prevLevelTitleAnimation :: Int -> Animation Picture
prevLevelTitleAnimation n = areverse (levelTitleAnimation n)
                         <> static blank

nextLevelTitleAnimation :: Int -> Animation Picture
nextLevelTitleAnimation n = levelTitleAnimation n
                         <> static blank


-- rotate into and out of view

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

rotateIntoView :: Picture -> Animation Picture
rotateIntoView p = rotatePopup <$> interpolate fadeDuration (-1) 0
                               <*> pure p

rotateAway :: Picture -> Animation Picture
rotateAway p = rotatePopup <$> interpolate fadeDuration 0 1
                           <*> pure p
