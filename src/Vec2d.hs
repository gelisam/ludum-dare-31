{-# LANGUAGE DeriveFunctor #-}
module Vec2d where

import Control.Applicative


data V a = V
  { vx :: a
  , vy :: a
  } deriving (Show, Eq, Functor)

instance Applicative V where
    pure x = V x x
    V f g <*> V x y = V (f x) (g y)

instance Num a => Num (V a) where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    abs = liftA abs
    signum = liftA signum
    fromInteger = pure . fromInteger
    negate = liftA negate

instance Fractional a => Fractional (V a) where
    fromRational = pure . fromRational
    recip = liftA recip
    (/) = liftA2 (/)

up, down, left, right :: Num a => V a
up    = V   0   1
down  = V   0 (-1)
left  = V (-1)  0
right = V   1   0

atL :: [a] -> Int -> Maybe a
atL (x:_) 0 = Just x
atL (_:xs) i | i > 0 = atL xs (i-1)
atL _ _ = Nothing

atV :: [[a]] -> V Int -> Maybe a
atV xss (V x y) = do
    xs <- xss `atL` y
    xs `atL` x
