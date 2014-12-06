{-# LANGUAGE ScopedTypeVariables #-}
module TitleScreen where

import Data.Monoid
import Graphics.Gloss
import Graphics.Gloss.Interface.FRP.ReactiveBanana
import Graphics.Gloss.Interface.Pure.Game hiding (Event)
import Reactive.Banana
import Reactive.Banana.Frameworks

import Animation
import Data.Bool.Extra
import Graphics.Gloss.Extra
import Input
import InputBlocking
import Popup
import Reactive.Banana.Animation
import Types


titleScreen :: forall t. Frameworks t
            => Behavior t Float
            -> Event t InputEvent
            -> InputBlocking t Picture
titleScreen time inputEvent = inputBlockingTitleScreen
  where
    -- keypress event
    
    anyKeyEvent :: Event t Key
    anyKeyEvent = whenE isTitleScreenUp $ keydownEvent inputEvent
    
    isTitleScreenUp :: Behavior t Bool
    isTitleScreenUp = stepper True $ False <$ anyKeyEvent
    
    
    -- combine white filter + text
    
    fadingTitleScreen :: Animated t Picture
    fadingTitleScreen = mappend <$> fadingWhiteFilter
                                <*> fadingText
    
    
    -- blink while not fading, block until disposed.
    
    blinkingTitleScreen :: Behavior t Picture
    blinkingTitleScreen = mappend staticWhiteFilter <$> blinkingText
    
    inputBlockingTitleScreen :: InputBlocking t Picture
    inputBlockingTitleScreen = InputBlocking possiblyBlinking
                                             isTitleScreenUp
                                             (() <$ anyKeyEvent)
      where
        possiblyBlinking :: Behavior t Picture
        possiblyBlinking = if_then_else <$> shouldBlink
                                        <*> blinkingTitleScreen
                                        <*> animatedValue fadingTitleScreen
        
        shouldBlink :: Behavior t Bool
        shouldBlink = (&&) <$> isTitleScreenUp
                           <*> (not <$> isAnimating fadingTitleScreen)
    
    
    -- white filter

    fadingWhiteFilter :: Animated t Picture
    fadingWhiteFilter = animateB time staticWhiteFilter
                      $ whiteFilterAnimation <$ anyKeyEvent
      where
        whiteFilterAnimation :: Animation Picture
        whiteFilterAnimation = makeWhiteFilter <$> fadeOut
        
        fadeOut :: Animation Float
        fadeOut = interpolate 2 1 0
    
    
    -- text
    
    fadingText :: Animated t Picture
    fadingText = animateB time staticText
               $ rotateAway staticText <$ anyKeyEvent
    
    blinkingText :: Behavior t Picture
    blinkingText = (commonText <>) <$> blinkingMessage
    
    staticText :: Picture
    staticText = commonText <> staticMessage
    
    commonText :: Picture
    commonText = title <> subtitle
    
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
    
    blinkingMessage :: Behavior t Picture
    blinkingMessage = guardPicture <$> (animationValue True (blinking 1 0.3) <$> time)
                                   <*> pure staticMessage
    
    staticMessage :: Picture
    staticMessage = translate (-250) (-100)
                  $ scale 0.3 0.3
                  $ text "Press any key to begin!"
