{-# LANGUAGE RecordWildCards, ScopedTypeVariables, TupleSections #-}
module Main where

import Data.Array
import Data.Maybe
import Data.Traversable
import Graphics.Gloss
import Graphics.Gloss.Interface.FRP.ReactiveBanana
import Reactive.Banana
import Reactive.Banana.Frameworks

import Controls
import Ending
import GameAnimation
import GameLogic
import Graphics
import InputBlocking
import LevelData
import Popup
import Sprites
import TitleScreen
import Types
import Vec2d


mainBanana :: forall t. Frameworks t
           => Sprites
           -> Event t ()
           -> Behavior t Float
           -> Event t InputEvent
           -> Moment t (Behavior t Picture)
mainBanana sprites tick time inputEvent = return picture
  where
    -- player movement
    
    canMove :: Behavior t Bool
    canMove = not <$> inputIsBlocked
    
    dirEvent :: Event t (V Int)
    dirEvent = whenE canMove
             $ directionalInput (inputUnblocked inputBlockingPlayer) inputEvent
    
    potentialWalkTilePos :: Event t TilePos
    potentialWalkTilePos = (+) <$> playerTilePos <@> dirEvent
    
    validMove :: Event t Move
    validMove = filterJust $ validateMove <$> stage <*> inventory <@> potentialWalkTilePos
    
    walkTilePos :: Event t TilePos
    walkTilePos = mTilePos <$> validMove
    
    walkTile :: Event t Tile
    walkTile = mNewTile <$> validMove
    
    
    -- consequences of player movement
    
    tileChange :: Event t TileChange
    tileChange = go <$> validMove
      where
        go :: Move -> TileChange
        go (Move {..}) = (mTilePos, mNewTile)
    
    inventoryChange :: Event t InventoryChanges
    inventoryChange = (mInventoryChanges <$> validMove)
              `union` levelCausedInventoryChange
    
    startEvent :: Event t ()
    startEvent = () <$ filterE (== Start) walkTile
    
    goalEvent :: Event t ()
    goalEvent = () <$ filterE (== Goal) walkTile
    
    
    -- level changes
    
    nextLevel :: Event t LevelNumber
    nextLevel = whenE ((lastLevel >) <$> levelNumber)
              $ (+1) <$> levelNumber <@ goalEvent
    
    theEnd :: Event t ()
    theEnd = whenE ((lastLevel ==) <$> levelNumber)
           $ () <$ goalEvent
    
    prevLevel :: Event t LevelNumber
    prevLevel = whenE ((0 <) <$> levelNumber)
              $ subtract 1 <$> levelNumber <@ startEvent
    
    showTitleScreen :: Event t ()
    showTitleScreen = whenE ((0 ==) <$> levelNumber)
                    $ () <$ startEvent
    
    nextWarpTilePos :: Behavior t TilePos
    nextWarpTilePos = stepper (error "nextWarpTilePos")
                            $ (startTilePos <$ nextLevel)
                      `union` (goalTilePos  <$ prevLevel)
    
    nextLevelChanges :: Behavior t (Bool, LevelChanges)
    nextLevelChanges = stepper (error "nextLevelChanges")
                             $ ((True,) . lLevelChanges . (levelData !!) <$> levelNumber <@ nextLevel)
                       `union` ((False,) <$> head <$> accumulatedChanges <@ prevLevel)
    
    nextLevelCausedInventoryChanges :: Behavior t InventoryChanges
    nextLevelCausedInventoryChanges = stepper []
                                    $ filterE (not . null)
                                    $ keysToRemove . subtract 1 <$> levelNumber <@ prevLevel
      where
        keysToRemove :: LevelNumber -> InventoryChanges
        keysToRemove n = ConsumeKey <$> keyChanges
          where
            allChanges :: LevelChanges
            allChanges = lLevelChanges (levelData !! n)
            
            tileChanges :: [Tile]
            tileChanges = fmap snd allChanges
            
            keyChanges :: [KeyNumber]
            keyChanges = mapMaybe isKey tileChanges
            
            isKey :: Tile -> Maybe KeyNumber
            isKey (Key k) = Just k
            isKey _ = Nothing
    
    warpTilePos :: Event t TilePos
    warpTilePos = nextWarpTilePos <@ inputUnblocked inputBlockingLevelPopup
    
    levelChanges :: Event t (Bool, LevelChanges)
    levelChanges = nextLevelChanges <@ inputUnblocked inputBlockingLevelPopup
    
    levelCausedInventoryChange :: Event t InventoryChanges
    levelCausedInventoryChange = nextLevelCausedInventoryChanges <@ inputUnblocked inputBlockingLevelPopup
    
    
    -- popup stuff
    
    inputBlockingTitleScreen :: InputBlocking t Picture
    inputBlockingTitleScreen = titleScreen showTitleScreen tick time inputEvent
    
    inputBlockingLevelPopup :: InputBlocking t Picture
    inputBlockingLevelPopup = blockInputB tick time blank
                            $ (prevLevelPopupAnimation <$> prevLevel)
                      `union` (nextLevelPopupAnimation <$> nextLevel)
    
    inputBlockingEnding :: InputBlocking t Picture
    inputBlockingEnding = blockInputB tick time blank
                        $ endingAnimation <$ theEnd
    
    
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
    
    inputBlockingStage :: InputBlocking t Stage
    inputBlockingStage = blockInputB tick time initialStage
                                   $ (blinkingObjectAnimation <$> stage <@> (snd <$> levelChanges))
                             `union` (tileChangingAnimation <$> stage <@> tileChange)
    
    inputIsBlocked :: Behavior t Bool
    inputIsBlocked = or <$> sequenceA [ isBlockingInput inputBlockingPlayer
                                      , isBlockingInput inputBlockingStage
                                      , isBlockingInput inputBlockingTitleScreen
                                      , isBlockingInput inputBlockingLevelPopup
                                      , isBlockingInput inputBlockingEnding
                                      ]
    
    
    -- debug stuff
    
    debugEvent :: Event t String
    debugEvent = never
    
    
    -- construct the game state for this frame
    
    levelNumber :: Behavior t LevelNumber
    levelNumber = stepper 0 $ prevLevel
                      `union` nextLevel
    
    stage :: Behavior t Stage
    stage = inputBlockingValue inputBlockingStage
    
    inventory :: Behavior t Inventory
    inventory = accumB []
              $ changeInventory <$> inventoryChange
    
    playerTilePos :: Behavior t TilePos
    playerTilePos = stepper startTilePos $ walkTilePos
                                   `union` warpTilePos
    
    playerGraphics :: Behavior t PlayerGraphics
    playerGraphics = inputBlockingValue inputBlockingPlayer
    
    accumulatedChanges :: Behavior t [LevelChanges]
    accumulatedChanges = accumB [] $ go <$> stage <@> levelChanges
      where
        go :: Stage -> (Bool, LevelChanges) -> [LevelChanges] -> [LevelChanges]
        go _ (False, _) = tail
        go stage' (True, xs) = rememberOldTiles xs
          where
            rememberOldTiles :: LevelChanges -> [LevelChanges] -> [LevelChanges]
            rememberOldTiles = (:)
                             . fmap (rememberOldTile . fst)
              where
                rememberOldTile :: TilePos -> LevelChange
                rememberOldTile tilePos = (tilePos, stage' ! tilePos)
    
    debugMessages :: Behavior t [String]
    debugMessages = accumB [] $ go <$> debugEvent
      where
        go :: String -> [String] -> [String]
        go x = take 10 . (x:)
    
    gameState :: Behavior t GameState
    gameState = GameState <$> levelNumber
                          <*> stage
                          <*> inventory
                          <*> playerTilePos
                          <*> playerGraphics
                          <*> accumulatedChanges
                          <*> debugMessages
    
    
    -- this frame's graphics
    
    picture :: Behavior t Picture
    picture = pictures <$> sequenceA [ renderGameState sprites <$> gameState
                                     , inputBlockingValue inputBlockingTitleScreen
                                     , inputBlockingValue inputBlockingLevelPopup
                                     , inputBlockingValue inputBlockingEnding
                                     ]

main :: IO ()
main = do
    sprites <- loadSprites "images"
    playBanana (InWindow gameTitle (640, 480) (800, 50))
               white
               60
               (mainBanana sprites)
