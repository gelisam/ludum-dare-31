{-# LANGUAGE ScopedTypeVariables #-}
module TitleScreen where

import Data.Monoid
import Data.Traversable
import Graphics.Gloss
import Graphics.Gloss.Interface.FRP.ReactiveBanana
import Reactive.Banana
import Reactive.Banana.Frameworks

import Animation
import Graphics.Gloss.Extra
import Popup
import Types


titleScreen :: forall t. Frameworks t
            => Behavior t Float
            -> Event t InputEvent
            -> (Behavior t Bool, Behavior t Picture)
titleScreen time inputEvent = (isVisible, picture)
  where
    alphaAnimation :: Animation Float
    alphaAnimation = interpolate 0.75 1 0
    
    alpha :: Behavior t Float
    alpha = animatedValue 0 alphaAnimation <$> time
    
    isVisible :: Behavior t Bool
    isVisible = isAnimating alphaAnimation <$> time
    
    picture :: Behavior t Picture
    picture = fadingTitleScreen
    
    fadingTitleScreen :: Behavior t Picture
    fadingTitleScreen = mappend <$> whiteFilter
                                <*> movingText
    
    whiteFilter :: Behavior t Picture
    whiteFilter = color <$> white'
                        <*> pure (rectangleSolid 640 480)
      where
        white' :: Behavior t Color
        white' = makeColor 1 1 1 <$> (0.9 *) <$> alpha
    
    movingText :: Behavior t Picture
    movingText = rotateAway <$> ((1 -) <$> alpha)
                            <*> (pictures <$> sequenceA textParts)
    
    textParts :: [Behavior t Picture]
    textParts = [ pure title
                , pure subtitle
                , flickeringMessage
                ]
      where
        title :: Picture
        title = translate (-309) 100
                $ scale 0.5 0.5
                $ text gameTitle
        
        subtitle :: Picture
        subtitle = translate (-290) 50
                 $ scale 0.2 0.2
                 $ text "Samuel Gelineau's entry for Ludum Dare 31"
                <> accent
          where
            accent :: Picture
            accent = line [(x, y), (x + dx, y + dy)]
              where
                (x, y) = (650, 80)
                (dx, dy) = (20, 10)
        
        flickeringMessage :: Behavior t Picture
        flickeringMessage = flickeringPicture <$> (animatedValue True (blinking 1 0.3) <$> time)
                                              <*> pure message
          where
            message :: Picture
            message = translate (-250) (-100)
                         $ scale 0.3 0.3
                         $ text "Press any key to begin!"
