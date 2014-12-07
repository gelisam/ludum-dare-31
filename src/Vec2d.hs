{-# LANGUAGE DeriveFunctor #-}
module Vec2d where

import Control.Applicative
import Data.Ix


data V a = V
  { vx :: a
  , vy :: a
  } deriving (Show, Eq, Ord, Ix, Functor)

runV :: V a -> (a, a)
runV (V x y) = (x, y)

runRange :: (V a, V a ) -> ((a,a), (a,a))
runRange (lo, hi) = (runV lo, runV hi)

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


-- I should probably use an Array...

atL :: [a] -> Int -> Maybe a
atL (x:_) 0 = Just x
atL (_:xs) i | i > 0 = atL xs (i-1)
atL _ _ = Nothing

atV :: [[a]] -> V Int -> Maybe a
atV xss (V x y) = do
    xs <- xss `atL` y
    xs `atL` x

setAtL :: Int -> a -> [a] -> [a]
setAtL 0 v (_:xs) = v : xs
setAtL i v (x:xs) = x : setAtL (i-1) v xs
setAtL _ _ [] = error "out of bounds"

setAtV :: V Int -> a -> [[a]] -> [[a]]
setAtV (V x y) v xss = setAtL y (setAtL x v (xss !! y)) xss
