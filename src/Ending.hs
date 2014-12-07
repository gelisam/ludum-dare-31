module Ending where

import Control.Applicative
import Data.Monoid
import Graphics.Gloss

import Animation
import Graphics.Gloss.Extra
import InputBlocking
import Popup
import Vec2d


endingAnimation :: InputBlockingAnimation Picture
endingAnimation = inputBlockingAnimation
                $ mappend fadeToWhite
                $ mappend staticWhite
                       <$> rollCredits
                        <> decelerate rollEndingScreen
                        <> static endingScreen
  where
    fadeToWhite = makeWhiteFilter <$> interpolate 5 0 1
    staticWhite = makeWhiteFilter 1
    
    rollCredits = mempty
    rollEndingScreen = translate 0 <$> interpolate 4 (-480) 0
                                   <*> pure endingScreen
    
    endingScreen = rectBoldPicture (V 8 2)
                 $ translate (-270) 0
                 $ text "The End"
