module GameAnimation where

import Control.Applicative
import Data.Monoid

import Animation
import InputBlocking
import Types


walkAnimation :: ScreenPos -> ScreenPos -> InputBlockingAnimation PlayerGraphics
walkAnimation screenPos1 screenPos2 = inputBlockingAnimation
                                    $ PlayerGraphics True <$> screenPos
  where
    screenPos :: Animation ScreenPos
    screenPos = interpolate 0.05 screenPos1 screenPos2

warpAnimation :: ScreenPos -> ScreenPos -> InputBlockingAnimation PlayerGraphics
warpAnimation screenPos1 screenPos2 = inputBlockingAnimation warping
                                   <> inputAllowingAnimation arrival
  where
    warping :: Animation PlayerGraphics
    warping = PlayerGraphics <$> visibility <*> screenPos
    
    arrival :: Animation PlayerGraphics
    arrival = idle 0 $ PlayerGraphics True screenPos2
    
    visibility :: Animation Bool
    visibility = flickering 0.1
    
    screenPos :: Animation ScreenPos
    screenPos = interpolate 1 screenPos1 screenPos2
