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
import Reactive.Banana.Animation
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
    
    newTilePos :: Event t TilePos
    newTilePos = (+) <$> playerTilePos <@> dirEvent
    
    newTile :: Event t Tile
    newTile = filterJust $ atV <$> stage <@> newTilePos
    
    
    -- consequences of player movement
    
    startEvent :: Event t ()
    startEvent = () <$ filterE (== Start) newTile
    
    goalEvent :: Event t ()
    goalEvent = () <$ filterE (== Goal) newTile
    
    
    -- animation stuff
    
    playerMovement :: Event t (Animation ScreenPos)
    playerMovement = interpolate 0.05 <$> oldPos <@> newPos
      where
        oldPos :: Behavior t ScreenPos
        oldPos = fmap fromIntegral <$> playerTilePos
        
        newPos :: Event t ScreenPos
        newPos = fmap fromIntegral <$> newTilePos
    
    animatedPlayerScreenPos :: Animated t ScreenPos
    animatedPlayerScreenPos = animateB time startScreenPos playerMovement
    
    animatedPlayer :: Animated t PlayerGraphics
    animatedPlayer = PlayerGraphics True <$> animatedPlayerScreenPos
    
    animationInProgress :: Behavior t Bool
    animationInProgress = or <$> sequenceA [ isAnimating animatedPlayer
                                           , isAnimating animatedTitleScreen
                                           ]
    
    
    -- debug stuff
    
    debugEvent :: Event t String
    debugEvent = ("next level" <$ goalEvent)
         `union` ("prev level" <$ startEvent)
    
    
    -- construct the game state for this frame
    
    levelNumber :: Behavior t LevelNumber
    levelNumber = accumB 0 $ ((+ 1) <$ goalEvent)
                     `union` (subtract 1 <$ startEvent)
    
    stage :: Behavior t Stage
    stage = pure initialStage
    
    playerTilePos :: Behavior t TilePos
    playerTilePos = accumB startTilePos $ (+) <$> dirEvent
    
    playerGraphics :: Behavior t PlayerGraphics
    playerGraphics = animatedValue animatedPlayer
    
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
    
    animatedTitleScreen :: Animated t Picture
    animatedTitleScreen = titleScreen time inputEvent
    
    picture :: Behavior t Picture
    picture = pictures <$> sequenceA [ renderGameState <$> gameState
                                     , animatedValue animatedTitleScreen
                                     ]

main :: IO ()
main = playBanana (InWindow gameTitle (640, 480) (800, 50))
                  white
                  60
                  mainBanana
