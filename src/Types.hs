module Types where

import Data.Array

import Vec2d


type LevelNumber = Int
type KeyNumber = Int

data Tile = Start
          | Goal
          | Empty
          | Floor
          | Wall
          | XWall
          | LockedDoor
          | UnlockedDoor
          | Key KeyNumber
  deriving (Show, Eq)

type TilePos = V Int
type ScreenPos = V Float

type Stage = Array TilePos Tile

type Inventory = [KeyNumber]
data InventoryChange = ReceiveKey KeyNumber
                     | ConsumeKey KeyNumber
  deriving (Show, Eq)
type InventoryChanges = [InventoryChange]

data Move = Move
  { mNewTile :: Tile
  , mTilePos :: TilePos
  , mScreenPos :: ScreenPos
  , mInventoryChanges :: InventoryChanges
  }

type LevelChange = (TilePos, Tile)
type LevelChanges = [LevelChange]

data PlayerGraphics = PlayerGraphics
  { gPlayerVisible      :: Bool
  , gPlayerScreenPos    :: ScreenPos
  } deriving (Show, Eq)

data GameState = GameState
  { gLevelNumber        :: LevelNumber
  , gStage              :: Stage
  , gInventory          :: Inventory
  , gPlayerTilePos      :: TilePos
  , gPlayerGraphics     :: PlayerGraphics
  , gAccumulatedChanges :: [LevelChanges]
  , gDebugMessages      :: [String]
  } deriving (Show, Eq)
