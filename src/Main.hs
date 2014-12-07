{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.Traversable
import Graphics.Gloss
import Graphics.Gloss.Interface.FRP.ReactiveBanana
import Reactive.Banana
import Reactive.Banana.Frameworks
import Text.Printf

import GameAnimation
import Graphics
import Input
import InputBlocking
import Popup
import TitleScreen
import Types
import Vec2d


mainBanana :: forall t. Frameworks t
           => Event t ()
           -> Behavior t Float
           -> Event t InputEvent
           -> Moment t (Behavior t Picture)
mainBanana tick time inputEvent = return picture
  where
    -- player movement
    
    canMove :: Behavior t Bool
    canMove = not <$> inputIsBlocked
    
    dirEvent :: Event t (V Int)
    dirEvent = whenE canMove $ filterJust $ keydown2dir <$> inputEvent
    
    walkTilePos :: Event t TilePos
    walkTilePos = (+) <$> playerTilePos <@> dirEvent
    
    walkTile :: Event t Tile
    walkTile = filterJust $ atV <$> stage <@> walkTilePos
    
    
    -- consequences of player movement
    
    startEvent :: Event t ()
    startEvent = () <$ filterE (== Start) walkTile
    
    goalEvent :: Event t ()
    goalEvent = () <$ filterE (== Goal) walkTile
    
    
    -- level changes
    
    nextLevel :: Event t LevelNumber
    nextLevel = (+1) <$> levelNumber <@ goalEvent
    
    prevLevel :: Event t LevelNumber
    prevLevel = whenE ((0 <) <$> levelNumber)
              $ subtract 1 <$> levelNumber <@ startEvent
    
    nextWarpTilePos :: Behavior t TilePos
    nextWarpTilePos = stepper undefined
                            $ (startTilePos <$ nextLevel)
                      `union` (goalTilePos  <$ prevLevel)
    
    warpTilePos :: Event t TilePos
    warpTilePos = nextWarpTilePos
               <@ inputUnblocked inputBlockingLevelPopup
    
    
    -- popup stuff
    
    inputBlockingTitleScreen :: InputBlocking t Picture
    inputBlockingTitleScreen = titleScreen time inputEvent
    
    inputBlockingLevelPopup :: InputBlocking t Picture
    inputBlockingLevelPopup = blockInputB tick time blank
                            $ (prevLevelPopupAnimation <$> prevLevel)
                      `union` (nextLevelPopupAnimation <$> nextLevel)
    
    
    -- animation stuff
    
    playerScreenPos :: Behavior t ScreenPos
    playerScreenPos = fmap fromIntegral <$> playerTilePos
    
    walkScreenPos :: Event t ScreenPos
    walkScreenPos = fmap fromIntegral <$> walkTilePos
    
    warpScreenPos :: Event t ScreenPos
    warpScreenPos = fmap fromIntegral <$> warpTilePos
    
    inputBlockingPlayer :: InputBlocking t PlayerGraphics
    inputBlockingPlayer = blockInputB tick time initialPlayerGraphics
                                 $ (walkAnimation <$> playerScreenPos <@> walkScreenPos)
                           `union` (warpAnimation <$> playerScreenPos <@> warpScreenPos)
    
    inputIsBlocked :: Behavior t Bool
    inputIsBlocked = or <$> sequenceA [ isBlockingInput inputBlockingPlayer
                                      , isBlockingInput inputBlockingTitleScreen
                                      , isBlockingInput inputBlockingLevelPopup
                                      ]
    
    
    -- debug stuff
    
    debugEvent :: Event t String
    debugEvent = (printf "next level: %d" <$> nextLevel)
         `union` (printf "prev level: %d" <$> prevLevel)
    
    
    -- construct the game state for this frame
    
    levelNumber :: Behavior t LevelNumber
    levelNumber = stepper 0 $ prevLevel
                      `union` nextLevel
    
    stage :: Behavior t Stage
    stage = pure initialStage
    
    playerTilePos :: Behavior t TilePos
    playerTilePos = stepper startTilePos $ walkTilePos
                                   `union` warpTilePos
    
    playerGraphics :: Behavior t PlayerGraphics
    playerGraphics = inputBlockingValue inputBlockingPlayer
    
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
    
    picture :: Behavior t Picture
    picture = pictures <$> sequenceA [ renderGameState <$> gameState
                                     , inputBlockingValue inputBlockingTitleScreen
                                     , inputBlockingValue inputBlockingLevelPopup
                                     ]

main :: IO ()
main = playBanana (InWindow gameTitle (640, 480) (800, 50))
                  white
                  60
                  mainBanana
