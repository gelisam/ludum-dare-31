{-# LANGUAGE ScopedTypeVariables #-}
module Reactive.Banana.Animation where

import Reactive.Banana
import Reactive.Banana.Frameworks

import Animation


data Animated a = Animated
  { animatedValue :: a
  , isAnimating :: Bool
  } deriving (Show, Eq)

animateB :: forall a t. Frameworks t
         => Behavior t Float
         -> a
         -> Event t (Animation a)
         -> Behavior t (Animated a)
animateB time x0 startAnim = Animated <$> currentValue
                                      <*> isCurrentAnimationInProgress
  where
    currentValue :: Behavior t a
    currentValue = animationValue <$> idleValue
                                  <*> currentAnimation
                                  <*> localTime
    
    isCurrentAnimationInProgress :: Behavior t Bool
    isCurrentAnimationInProgress = isAnimationInProgress <$> currentAnimation
                                                         <*> localTime
    
    idleValue :: Behavior t a
    idleValue = accumB x0 $ const . lastFrame <$> startAnim
    
    lastFrame :: Animation a -> a
    lastFrame anim | isInfinite d = undefined  -- we won't have to hold the last frame
                   | otherwise    = snapshot anim d
      where
        d = duration anim
    
    currentAnimation :: Behavior t (Animation a)
    currentAnimation = accumB mempty $ const <$> startAnim
    
    startTime :: Behavior t Float
    startTime = accumB 0 $ const <$> time <@ startAnim
    
    localTime :: Behavior t Float
    localTime = (-) <$> time <*> startTime
