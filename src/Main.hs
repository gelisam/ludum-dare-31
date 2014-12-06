{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.Traversable
import Graphics.Gloss
import Graphics.Gloss.Interface.FRP.ReactiveBanana
import Reactive.Banana
import Reactive.Banana.Frameworks

import Animation
import Graphics
import Input
import TitleScreen
import Types
import Vec2d


mainBanana :: forall t. Frameworks t
           => Behavior t Float
           -> Event t InputEvent
           -> Moment t (Behavior t Picture)
mainBanana time inputEvent = return picture
  where
    -- player movement
    
    canMove :: Behavior t Bool
    canMove = not <$> animationInProgress
    
    dirEvent :: Event t (V Int)
    dirEvent = whenE canMove $ filterJust $ keydown2dir <$> inputEvent
    
    newPos :: Event t TilePos
    newPos = (+) <$> playerTilePos <@> dirEvent
    
    newTile :: Event t Tile
    newTile = filterJust $ atV <$> stage <@> newPos
    
    
    -- consequences of player movement
    
    startEvent :: Event t ()
    startEvent = const () <$> filterE (== Start) newTile
    
    goalEvent :: Event t ()
    goalEvent = const () <$> filterE (== Goal) newTile
    
    
    -- animation stuff
    
    playerAnimation :: Behavior t (Animation PlayerGraphics)
    playerAnimation = pure
                    $ PlayerGraphics <$> flickering 0.1
                                     <*> interpolate 1 (fmap fromIntegral goalPosition)
                                                       (fmap fromIntegral startPosition)
    
    animationInProgress :: Behavior t Bool
    animationInProgress = or <$> sequenceA [ isAnimationInProgress <$> playerAnimation <*> time
                                           , isTitleScreenAnimating
                                           ]
    
    
    -- debug stuff
    
    debugEvent :: Event t String
    debugEvent = (const "next level" <$> goalEvent)
         `union` (const "prev level" <$> startEvent)
    
    
    -- construct the game state for this frame
    
    levelNumber :: Behavior t LevelNumber
    levelNumber = accumB 0 $ (const (+ 1) <$> goalEvent)
                     `union` (const (subtract 1) <$> startEvent)
    
    stage :: Behavior t Stage
    stage = pure initialStage
    
    playerTilePos :: Behavior t TilePos
    playerTilePos = accumB startPosition $ (+) <$> dirEvent
    
    playerGraphics :: Behavior t PlayerGraphics
    playerGraphics = animationValue <$> (PlayerGraphics True <$> fmap fromIntegral <$> playerTilePos)
                                    <*> playerAnimation
                                    <*> time
    
    accumulatedChanges :: Behavior t [LevelChanges]
    accumulatedChanges = pure []
    
    debugMessages :: Behavior t [String]
    debugMessages = accumB [] $ (:) <$> debugEvent
    
    gameState :: Behavior t GameState
    gameState = GameState <$> levelNumber
                          <*> stage
                          <*> playerTilePos
                          <*> playerGraphics
                          <*> accumulatedChanges
                          <*> debugMessages
    
    
    -- this frame's graphics
    
    (isTitleScreenAnimating, renderTitleScreen) = titleScreen time inputEvent
    
    picture :: Behavior t Picture
    picture = pictures <$> sequenceA [ renderGameState <$> gameState
                                     , renderTitleScreen
                                     ]

main :: IO ()
main = playBanana (InWindow gameTitle (640, 480) (800, 50))
                  white
                  60
                  mainBanana
