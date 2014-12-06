{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.FRP.ReactiveBanana
import Reactive.Banana
import Reactive.Banana.Frameworks

import Graphics
import Input
import Types
import Vec2d


mainBanana :: forall t. Frameworks t
           => Event t Float
           -> Event t InputEvent
           -> Moment t (Behavior t Picture)
mainBanana _ inputEvent = return picture
  where
    dirEvent :: Event t (V Int)
    dirEvent = filterJust $ keydown2dir <$> inputEvent
    
    levelNumber :: Behavior t LevelNumber
    levelNumber = pure 0
    
    stage :: Behavior t Stage
    stage = pure initialStage
    
    player :: Behavior t Player
    player = accumB 0 ((+) <$> dirEvent)
    
    accumulatedChanges :: Behavior t [LevelChanges]
    accumulatedChanges = pure []
    
    gameState :: Behavior t GameState
    gameState = GameState <$> levelNumber <*> stage <*> player <*> accumulatedChanges
    
    picture :: Behavior t Picture
    picture = renderGameState <$> gameState

main :: IO ()
main = playBanana (InWindow "Ludum Dare 31" (640, 480) (800, 50))
                  white
                  0
                  mainBanana
