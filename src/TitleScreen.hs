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
import LevelData
import Popup


titleScreen :: forall t. Frameworks t
            => Event t ()
            -> Event t ()
            -> Behavior t Float
            -> Event t InputEvent
            -> InputBlocking t Picture
titleScreen showTitleScreen tick time inputEvent = inputBlockingBlinkingTitleScreen
  where
    -- keypress event
    
    anyKeyEvent :: Event t Key
    anyKeyEvent = whenE isWaitingForKey $ keydownEvent inputEvent
    
    isWaitingForKey :: Behavior t Bool
    isWaitingForKey = stepper True $ (False <$ anyKeyEvent)
                             `union` (True  <$ inputUnblocked inputBlockingFadeIn)
    
    inputBlockingFadeIn :: InputBlocking t Picture
    inputBlockingFadeIn = blockInputB tick time blank
                        $ fadeInTitleScreenAnimation <$ showTitleScreen
    
    inputBlockingNonBlinkingTitleScreen :: InputBlocking t Picture
    inputBlockingNonBlinkingTitleScreen = blockInputB tick time staticTitleScreen
                                        $ (fadeInTitleScreenAnimation <$ showTitleScreen)
                                  `union` (fadeOutTitleScreenAnimation <$ anyKeyEvent)
    
    
    -- combine white filter + text
    
    fadeInTitleScreenAnimation :: InputBlockingAnimation Picture
    fadeInTitleScreenAnimation = mappend <$> whiteFadeInAnimation
                                         <*> textFadeInAnimation
    
    fadeOutTitleScreenAnimation :: InputBlockingAnimation Picture
    fadeOutTitleScreenAnimation = mappend <$> whiteFadeOutAnimation
                                          <*> textFadeOutAnimation
    
    staticTitleScreen :: Picture
    staticTitleScreen = staticWhiteFilter <> staticText
    
    
    -- blink while not fading, block until disposed.
    
    blinkingAnimation :: InputBlockingAnimation Picture
    blinkingAnimation = mappend staticWhiteFilter <$> blinkingTextAnimation
    
    inputBlockingBlinkingTitleScreen :: InputBlocking t Picture
    inputBlockingBlinkingTitleScreen = inputBlockingNonBlinkingTitleScreen
                                     { inputBlockingValue = possiblyBlinking
                                     }
      where
        possiblyBlinking :: Behavior t Picture
        possiblyBlinking = if_then_else <$> isWaitingForKey
                                        <*> blinkingTitleScreen
                                        <*> inputBlockingValue inputBlockingNonBlinkingTitleScreen
        
        blinkingTitleScreen :: Behavior t Picture
        blinkingTitleScreen = inputBlockingAnimationValue staticTitleScreen blinkingAnimation <$> time
    
    
    -- text
    
    textFadeInAnimation :: InputBlockingAnimation Picture
    textFadeInAnimation = inputBlockingAnimation
                        $ rotateIntoView staticText
    
    textFadeOutAnimation :: InputBlockingAnimation Picture
    textFadeOutAnimation = inputAllowingAnimation
                         $ rotateAway staticText
                        <> static blank
    
    blinkingTextAnimation :: InputBlockingAnimation Picture
    blinkingTextAnimation = (commonText <>) <$> blinkingMessageAnimation
    
    staticText :: Picture
    staticText = commonText <> staticMessage
    
    commonText :: Picture
    commonText = title <> subtitle
    
    title :: Picture
    title = boldPicture
          $ translate (-312) 100
          $ scale 0.34 0.34
          $ blackText gameTitle
    
    subtitle :: Picture
    subtitle = translate (-290) 50
             $ scale 0.2 0.2
             $ blackText "Samuel Gelineau's entry for Ludum Dare 31"
            <> accent
      where
        accent :: Picture
        accent = line [(x, y), (x + dx, y + dy)]
          where
            (x, y) = (650, 80)
            (dx, dy) = (20, 10)
    
    blinkingMessageAnimation :: InputBlockingAnimation Picture
    blinkingMessageAnimation = inputBlockingAnimation
                    $ guardPicture <$> blinking 1 0.3
                                   <*> pure staticMessage
    
    staticMessage :: Picture
    staticMessage = translate (-250) (-100)
                  $ scale 0.3 0.3
                  $ blackText "Press any key to begin!"
