{-# LANGUAGE ScopedTypeVariables #-}
module Controls where

import Graphics.Gloss.Interface.FRP.ReactiveBanana
import Reactive.Banana
import Reactive.Banana.Frameworks

import Input
import Vec2d


type Dir2 = Int
type Dir4 = V Int
type Dir8 = V Int

forceDir2 :: Int -> Dir2
forceDir2 x | x < 0     = (-1)
            | x > 0     = 1
            | otherwise = 0

forceDir4 :: Bool -> V Int -> Dir4
forceDir4 favorVertical v | (x == 0 || y == 0) = V x y
                          | favorVertical      = V 0 y
                          | otherwise          = V x 0
  where
    V x y = forceDir8 v

forceDir8 :: V Int -> Dir8
forceDir8 (V x y) = V (forceDir2 x) (forceDir2 y)


directionalInput :: forall t. Frameworks t
                 => Event t ()
                 -> Event t InputEvent
                 -> Event t Dir4
directionalInput checkForInput inputEvent = nonZeroDir4Event
  where
    dirChange :: Event t (V Int -> V Int)
    dirChange = ((+)      <$> filterJust (keydown2dir <$> inputEvent))
        `union` (subtract <$> filterJust (keyup2dir <$> inputEvent))
    
    totalDir :: Behavior t (V Int)
    totalDir = accumB 0 dirChange
    
    favorVertical :: Behavior t Bool
    favorVertical = stepper True
                  $ (True  <$ upEvent inputEvent)
            `union` (True  <$ downEvent inputEvent)
            `union` (False <$ leftEvent inputEvent)
            `union` (False <$ rightEvent inputEvent)
    
    newDir :: Event t (V Int)
    newDir = flip ($) <$> totalDir <@> dirChange
    
    dirEvent :: Event t (V Int)
    dirEvent = newDir `union` (totalDir <@ checkForInput)
    
    dir4Event :: Event t Dir4
    dir4Event = forceDir4 <$> favorVertical
                          <@> dirEvent
    
    nonZeroDir4Event :: Event t Dir4
    nonZeroDir4Event = filterE (/= 0) dir4Event
