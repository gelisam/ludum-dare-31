{-# LANGUAGE DeriveFunctor, ScopedTypeVariables #-}
module InputBlocking where

import Data.Monoid
import Control.Applicative
import Reactive.Banana
import Reactive.Banana.Frameworks

import Animation


data InputBlockingAnimation a = InputBlockingAnimation
  { underlyingAnimation :: Animation a
  , inputBlockingDurations :: [(Bool, Float)]
  } deriving Functor

instance Monoid (InputBlockingAnimation a) where
    mempty = InputBlockingAnimation mempty mempty
    ibAnim1 `mappend` ibAnim2
      = InputBlockingAnimation (underlyingAnimation ibAnim1 `mappend` underlyingAnimation ibAnim2)
                               (inputBlockingDurations ibAnim1 `mappend` inputBlockingDurations ibAnim2)

instance Applicative InputBlockingAnimation where
    pure = inputAllowingAnimation . pure
    ibf <*> ibx
      = InputBlockingAnimation (underlyingAnimation ibf <*> underlyingAnimation ibx)
                               (inputBlockingDurations ibf `merge` inputBlockingDurations ibx)
      where
        merge :: [(Bool, Float)] -> [(Bool, Float)] -> [(Bool, Float)]
        merge xs1 [] = xs1
        merge [] xs2 = xs2
        merge ((isBlocking1,dt1):xs1) ((isBlocking2,dt2):xs2)
            | dt1 < dt2 = (isBlocking',dt1) : merge xs1 (x2:xs2)
            | dt1 > dt2 = (isBlocking',dt2) : merge (x1:xs1) xs2
            | otherwise = (isBlocking',dt1) : merge xs1 xs2
          where
            isBlocking' = isBlocking1 || isBlocking2
            dt1' = dt1 - dt2
            dt2' = dt2 - dt1
            x1 = (isBlocking1, dt1')
            x2 = (isBlocking2, dt2')

mkInputBlockingAnimation :: Bool -> Animation a -> InputBlockingAnimation a
mkInputBlockingAnimation isBlocking anim
  = InputBlockingAnimation anim [(isBlocking, duration anim)]

inputBlockingAnimation :: Animation a -> InputBlockingAnimation a
inputBlockingAnimation = mkInputBlockingAnimation True

inputAllowingAnimation :: Animation a -> InputBlockingAnimation a
inputAllowingAnimation = mkInputBlockingAnimation False


isAnimationBlockingInput :: InputBlockingAnimation a -> Float -> Bool
isAnimationBlockingInput = go . inputBlockingDurations
  where
    go :: [(Bool, Float)] -> Float -> Bool
    go [] _ = False
    go ((isBlocking, dt):xs) t | t < 0     = False
                               | t < dt    = isBlocking
                               | otherwise = go xs (t - dt)

inputBlockingAnimationValue :: a -> InputBlockingAnimation a -> Float -> a
inputBlockingAnimationValue idleValue = animationValue idleValue
                                      . underlyingAnimation

inputBlockingAnimationDuration :: InputBlockingAnimation a -> Float
inputBlockingAnimationDuration = duration
                               . underlyingAnimation


data InputBlocking t a = InputBlocking
  { inputBlockingValue :: Behavior t a
  , isBlockingInput :: Behavior t Bool
  , inputUnblocked :: Event t ()
  } deriving Functor

instance Applicative (InputBlocking t) where
    pure x = InputBlocking (pure x) (pure False) never
    af <*> ax = InputBlocking inputBlockingValue'
                              isBlockingInput'
                              inputUnblocked'
      where
        inputBlockingValue' = inputBlockingValue af <*> inputBlockingValue ax
        isBlockingInput' = (||) <$> isBlockingInput af <*> isBlockingInput ax
        inputUnblocked' = whenE (not <$> isBlockingInput')
                              $ inputUnblocked af
                        `union` inputUnblocked ax

blockInputB :: forall a t. Frameworks t
            => Event t ()
            -> Behavior t Float
            -> a
            -> Event t (InputBlockingAnimation a)
            -> InputBlocking t a
blockInputB tick time x0 startAnim = InputBlocking currentValue
                                                   isCurrentAnimationBlockingInput
                                                   inputUnblockedEvent
  where
    currentValue :: Behavior t a
    currentValue = inputBlockingAnimationValue <$> idleValue
                                               <*> currentAnimation
                                               <*> localTime
    
    isCurrentAnimationBlockingInput :: Behavior t Bool
    isCurrentAnimationBlockingInput = isAnimationBlockingInput <$> currentAnimation
                                                               <*> localTime
    
    inputUnblockedEvent :: Event t ()
    inputUnblockedEvent = whenE newlyUnblocked tick
      where
        newlyUnblocked = (&&) <$> inputWasBlocked
                              <*> (not <$> isCurrentAnimationBlockingInput)
    
    inputWasBlocked :: Behavior t Bool
    inputWasBlocked = stepper False $ isCurrentAnimationBlockingInput <@ tick
    
    idleValue :: Behavior t a
    idleValue = stepper x0 $ lastInputBlockingFrame <$> startAnim
    
    lastFrame :: Animation a -> a
    lastFrame anim | isInfinite d = undefined  -- we won't have to hold the last frame
                   | otherwise    = snapshot anim d
      where
        d = duration anim
    
    lastInputBlockingFrame :: InputBlockingAnimation a -> a
    lastInputBlockingFrame = lastFrame . underlyingAnimation
    
    currentAnimation :: Behavior t (InputBlockingAnimation a)
    currentAnimation = stepper mempty startAnim
    
    startTime :: Behavior t Float
    startTime = stepper 0 $ time <@ startAnim
    
    localTime :: Behavior t Float
    localTime = (-) <$> time <*> startTime
