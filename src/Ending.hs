module Ending where

import Data.Monoid
import Graphics.Gloss

import Animation
import InputBlocking
import Popup


endingAnimation :: InputBlockingAnimation Picture
endingAnimation = inputBlockingAnimation
                $ rotateIntoView endingScreen
               <> static endingScreen
  where
    endingScreen = translate (-300) 0
                 $ text "THE END"
