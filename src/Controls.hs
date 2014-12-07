{-# LANGUAGE ScopedTypeVariables #-}
module Controls where

import Control.Monad
import Graphics.Gloss.Interface.FRP.ReactiveBanana
import Reactive.Banana
import Reactive.Banana.Frameworks

import Input
import Vec2d


directionalInput :: forall t. Frameworks t
                 => Event t ()
                 -> Event t InputEvent
                 -> Event t (V Int)
directionalInput checkForInput inputEvent = dirEvent
  where
    dirChange :: Event t (V Int -> V Int)
    dirChange = ((+)      <$> filterJust (keydown2dir <$> inputEvent))
        `union` (subtract <$> filterJust (keyup2dir <$> inputEvent))
    
    totalDir :: Behavior t (V Int)
    totalDir = accumB 0 dirChange
    
    newDir :: Event t (V Int)
    newDir = flip ($) <$> totalDir <@> dirChange
    
    dirEvent :: Event t (V Int)
    dirEvent = filterJust
             $ validateV
           <$> newDir
       `union` (totalDir <@ checkForInput)
      where
        validateV :: V Int -> Maybe (V Int)
        validateV (V x y) = do
            V x' y' <- V <$> validateInt x <*> validateInt y
            guard (x' == 0 || y' == 0)
            guard (x' /= 0 || y' /= 0)
            return (V x' y')
        
        validateInt :: Int -> Maybe Int
        validateInt x | x < 0     = Just (-1)
                      | x > 0     = Just 1
                      | otherwise = Just 0
