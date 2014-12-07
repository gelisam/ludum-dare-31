module Popup where

import Data.Monoid
import Control.Applicative
import Graphics.Gloss
import Text.Printf

import Animation
import Graphics.Gloss.Extra
import InputBlocking
import LevelData
import Types


fadeDuration :: Float
fadeDuration = 0.75

idleDuration :: Float
idleDuration = 0.75

whiteFadeOutDuration :: Float
whiteFadeOutDuration = 2

whiteOpacity :: Float
whiteOpacity = 0.9


fadeInAnimation :: InputBlockingAnimation Float
fadeInAnimation = inputBlockingAnimation $ interpolate fadeDuration 0 whiteOpacity

fadeOutAnimation :: InputBlockingAnimation Float
fadeOutAnimation = inputAllowingAnimation $ interpolate whiteFadeOutDuration whiteOpacity 0


-- level popup (fade in, fade out)

staticLevelPopup :: Bool -> Int -> Picture
staticLevelPopup direction n = staticWhiteFilter <> levelText direction n

prevLevelPopupAnimation :: Int -> InputBlockingAnimation Picture
prevLevelPopupAnimation n = mappend <$> whitePopupAnimation
                                    <*> inputAllowingAnimation (prevLevelTextAnimation n)

nextLevelPopupAnimation :: Int -> InputBlockingAnimation Picture
nextLevelPopupAnimation n = mappend <$> whitePopupAnimation
                                    <*> inputAllowingAnimation (nextLevelTextAnimation n)


-- white filter

staticWhiteFilter :: Picture
staticWhiteFilter = makeWhiteFilter whiteOpacity

makeWhiteFilter :: Float -> Picture
makeWhiteFilter alpha = color white' (rectangleSolid 640 480)
  where
    white' :: Color
    white' = makeColor 1 1 1 alpha

whiteFadeInAnimation :: InputBlockingAnimation Picture
whiteFadeInAnimation = makeWhiteFilter <$> fadeInAnimation

whiteFadeOutAnimation :: InputBlockingAnimation Picture
whiteFadeOutAnimation = makeWhiteFilter <$> fadeOutAnimation

whiteIdleAnimation :: InputBlockingAnimation Picture
whiteIdleAnimation = inputBlockingAnimation
                       $ idle idleDuration
                       $ makeWhiteFilter whiteOpacity

whitePopupAnimation :: InputBlockingAnimation Picture
whitePopupAnimation = whiteFadeInAnimation
                   <> whiteIdleAnimation
                   <> whiteFadeOutAnimation


-- text

levelTitle :: Int -> Picture
levelTitle = translate (-125) 100
           . uscale 0.5
           . blackText
           . printf "Level %d"

levelSubtitle :: Bool -> Int -> Picture
levelSubtitle direction = translate (-125) 0
                        . uscale (scale' direction)
                        . blackText
                        . subtitle direction
  where
    scale' True  = 0.3
    scale' False = 0.2
    
    subtitle True  n = lForwardMessage (levelData !! (n - 1))
    subtitle False n = lBackwardMessage (levelData !! n)

levelText :: Bool -> Int -> Picture
levelText direction n = levelTitle n <> levelSubtitle direction n


levelTextAnimation :: Bool -> Int -> Animation Picture
levelTextAnimation direction n = decelerate (rotateIntoView t)
                              <> idle idleDuration t
                              <> accelerate (rotateAway t)
  where
    t = levelText direction n

prevLevelTextAnimation :: Int -> Animation Picture
prevLevelTextAnimation n = areverse (levelTextAnimation False n)
                        <> static blank

nextLevelTextAnimation :: Int -> Animation Picture
nextLevelTextAnimation n = levelTextAnimation True n
                        <> static blank


-- rotate into and out of view

-- -1 <= t <= 1
rotatePopup :: Float -> Picture -> Picture
rotatePopup t = translate 0 (-offset)
             . rotate angle
             . translate 0 offset
             . uscale factor
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
