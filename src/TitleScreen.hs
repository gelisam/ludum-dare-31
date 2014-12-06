{-# LANGUAGE ScopedTypeVariables #-}
module TitleScreen where

import Data.Monoid
import Data.Traversable
import Graphics.Gloss
import Reactive.Banana

import Animation
import Graphics.Gloss.Extra
import Types


titleScreen :: forall t. Behavior t Float -> Behavior t Picture
titleScreen time = pictures <$> sequenceA
                 [ pure whiteFilter
                 , pure title
                 , pure subtitle
                 , flickeringMessage
                 ]
  where
    title = translate (-309) 100
            $ scale 0.5 0.5
            $ text gameTitle
    
    subtitle = translate (-290) 50
             $ scale 0.2 0.2
             $ textWithAccent
    
    textWithAccent = text "Samuel Gelineau's entry for Ludum Dare 31"
                  <> accent
    
    accent = line [(x, y), (x + dx, y + dy)]
      where
        (x, y) = (650, 80)
        (dx, dy) = (20, 10)
    
    flickeringMessage :: Behavior t Picture
    flickeringMessage = flickeringPicture <$> (animatedValue True (blinking 1 0.3) <$> time)
                                          <*> pure startMessage
    
    startMessage = translate (-250) (-100)
                 $ scale 0.3 0.3
                 $ text "Press any key to begin!"
    
    whiteFilter = color (makeColor 1 1 1 0.9)
                $ rectangleSolid 640 480
