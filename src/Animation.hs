{-# LANGUAGE DeriveFunctor, RecordWildCards #-}
module Animation where

import Control.Applicative
import Data.Monoid

-- $setup
-- >>> let anim = idle 2 "foo" <> idle 2 "bar"


data Animation a = Animation
  { snapshot :: Float -> a
  , duration :: Float
  } deriving Functor

-- |
-- >>> sampleAnimation anim
-- ["foo","foo","bar","bar"]
sampleAnimation :: Animation a -> [a]
sampleAnimation anim | isInfinite (duration anim) = sampleAnimation (matchDuration 15 anim)
                     | duration anim <= 0 = []
                     | otherwise = snapshot anim 0
                                 : sampleAnimation (skip 1 anim)


-- |
-- >>> sampleAnimation mempty
-- []
-- >>> sampleAnimation $ anim <> idle 3 "baz"
-- ["foo","foo","bar","bar","baz","baz","baz"]
instance Monoid (Animation a) where
    mempty = Animation undefined 0
    anim1 `mappend` anim2 = Animation f (d1 + d2)
      where
        d1 = duration anim1
        d2 = duration anim2
        f t | t < d1    = snapshot anim1 t
            | otherwise = snapshot anim2 (t - d1)

-- |
-- >>> sampleAnimation $ mreplicate 3 anim
-- ["foo","foo","bar","bar","foo","foo","bar","bar","foo","foo","bar","bar"]
mreplicate :: Monoid m => Int -> m -> m
mreplicate n = mconcat . replicate n


-- |
-- >>> sampleAnimation ((,) <$> interpolate 10 0.0 10.0 <*> interpolate 5 0.0 1.0 :: Animation (Float,Float))
-- [(0.0,0.0),(1.0,0.2),(2.0,0.4),(3.0,0.6),(4.0,0.8)]
instance Applicative Animation where
    pure = static
    anim1 <*> anim2 = Animation (\t -> (snapshot anim1 t) (snapshot anim2 t))
                                (min (duration anim1) (duration anim2))


-- |
-- >>> sampleAnimation $ idle 4 "foo"
-- ["foo","foo","foo","foo"]
idle :: Float -> a -> Animation a
idle duration x = Animation (const x) duration

-- |
-- >>> sampleAnimation $ static "foo"
-- ["foo","foo","foo","foo","foo","foo","foo","foo","foo","foo","foo","foo","foo","foo","foo"]
static :: a -> Animation a
static x = idle infinity x
  where
    infinity = 1.0 / 0.0


-- |
-- >>> sampleAnimation $ skip 2 $ idle 4 "foo"
-- ["foo","foo"]
-- >>> sampleAnimation $ skip 3 anim
-- ["bar"]
-- >>> sampleAnimation $ skip 10000000 (static "foo")
-- ["foo","foo","foo","foo","foo","foo","foo","foo","foo","foo","foo","foo","foo","foo","foo"]
skip :: Float -> Animation a -> Animation a
skip dt (Animation {..}) = Animation (\t -> snapshot (t + dt))
                                     (duration - dt)


-- |
-- >>> sampleAnimation $ slowMotion 2 anim
-- ["foo","foo","foo","foo","bar","bar","bar","bar"]
-- >>> sampleAnimation $ slowMotion 1.5 anim
-- ["foo","foo","foo","bar","bar","bar"]
-- >>> sampleAnimation $ slowMotion 0.5 anim
-- ["foo","bar"]
-- >>> sampleAnimation $ slowMotion 2 (static "foo")
-- ["foo","foo","foo","foo","foo","foo","foo","foo","foo","foo","foo","foo","foo","foo","foo"]
slowMotion :: Float -> Animation a -> Animation a
slowMotion factor (Animation {..}) = Animation (\t -> snapshot (t / factor))
                                               (duration * factor)

-- |
-- >>> sampleAnimation $ fastForward 2 anim
-- ["foo","bar"]
-- >>> sampleAnimation $ fastForward 0.5 anim
-- ["foo","foo","foo","foo","bar","bar","bar","bar"]
-- >>> sampleAnimation $ fastForward 2 (static "foo")
-- ["foo","foo","foo","foo","foo","foo","foo","foo","foo","foo","foo","foo","foo","foo","foo"]
fastForward :: Float -> Animation a -> Animation a
fastForward factor (Animation {..}) = Animation (\t -> snapshot (t * factor))
                                                (duration / factor)

-- |
-- >>> sampleAnimation $ matchDuration 2 anim
-- ["foo","bar"]
-- >>> sampleAnimation $ matchDuration 3 anim
-- ["foo","foo","bar"]
-- >>> sampleAnimation $ matchDuration 5 anim
-- ["foo","foo","foo","bar","bar"]
-- >>> sampleAnimation $ matchDuration 5 (static "foo")
-- ["foo","foo","foo","foo","foo"]
matchDuration :: Float -> Animation a -> Animation a
matchDuration targetDuration anim | isInfinite d = Animation (snapshot anim) targetDuration
                                  | otherwise    = slowMotion (targetDuration / d) anim
  where
    d = duration anim


-- |
-- >>> sampleAnimation $ interpolate 4 0.0 1.0
-- [0.0,0.25,0.5,0.75]
interpolate :: Fractional a => Float -> a -> a -> Animation a
interpolate targetDuration x0 xZ = matchDuration targetDuration
                                 $ Animation (\t -> x0 + realToFrac t * (xZ - x0))
                                             1.0


-- |
-- >>> sampleAnimation $ accelerate anim
-- ["foo","foo","foo","bar"]
-- >>> sampleAnimation $ accelerate (static "foo")
-- ["foo","foo","foo","foo","foo","foo","foo","foo","foo","foo","foo","foo","foo","foo","foo"]
accelerate :: Animation a -> Animation a
accelerate anim = anim'''
  where
    anim' = matchDuration 1.0 anim
    anim'' = Animation (\t -> snapshot anim' (t*t)) 1.0
    anim''' = matchDuration (duration anim) anim''

-- |
-- >>> sampleAnimation $ decelerate anim
-- ["foo","bar","bar","bar"]
-- >>> sampleAnimation $ decelerate (static "foo")
-- ["foo","foo","foo","foo","foo","foo","foo","foo","foo","foo","foo","foo","foo","foo","foo"]
decelerate :: Animation a -> Animation a
decelerate anim = anim'''
  where
    anim' = matchDuration 1.0 anim
    anim'' = Animation (\t -> snapshot anim' (sqrt t)) 1.0
    anim''' = matchDuration (duration anim) anim''


isAnimating :: Animation a -> Float -> Bool
isAnimating anim t = t < duration anim

animatedValue :: a -> Animation a -> Float -> a
animatedValue idleValue anim t | isAnimating anim t = snapshot anim t
                               | otherwise          = idleValue
