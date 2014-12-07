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
                        <> static endingScreen
  where
    fadeToWhite = makeWhiteFilter <$> interpolate 5 0 1
    staticWhite = makeWhiteFilter 1
    
    rollCredits = translate 0 <$> interpolate 20 (-750) 0
                              <*> pure credits
    
    credits = mappend endingScreen
            $ translate (-210) 240
            $ pictureCol 20
            $ reverse
            [ title "Credits"
            , blank
            , blank
            , sectionTitle "Programming"
            , author
            , blank
            , sectionTitle "Art"
            , author
            , blank
            , blank
            , text' "Created in 48h for Ludum Dare 31, with"
            , text' "the theme \"Entire Game on One Screen\"."
            ]
      where
        title :: String -> Picture
        title = rectBoldPicture (V 2 1) . text'
        
        sectionTitle :: String -> Picture
        sectionTitle = rectBoldPicture (V 1 1) . text'
        
        text' :: String -> Picture
        text' = scale 0.15 0.15 . text
        
        author :: Picture
        author = text' "Samuel Gelineau"
              <> accent
        
        accent :: Picture
        accent = line [(x, y), (x + dx, y + dy)]
          where
            (x, y) = (97, 12)
            (dx, dy) = (4, 2)
    
    endingScreen = rectBoldPicture (V 8 2)
                 $ translate (-270) 0
                 $ text "The End"