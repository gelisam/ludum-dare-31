module Types where

import Vec2d


gameTitle :: String
gameTitle = "More of the Same"


type LevelNumber = Int
type KeyNumber = Int

data Tile = Start
          | Goal
          | Floor
          | Wall
          | LockedDoor
          | UnlockedDoor
          | Key KeyNumber
  deriving (Show, Eq)

type Stage = [[Tile]]

type TilePos = V Int
type ScreenPos = V Float

type Inventory = [KeyNumber]

data InventoryChange = ReceiveKey KeyNumber
                     | ConsumeKey KeyNumber
  deriving (Show, Eq)

data Move = Move
  { mTile :: Tile
  , mTilePos :: TilePos
  , mScreenPos :: ScreenPos
  , mInventoryChanges :: InventoryChanges
  }

type LevelChanges = [(TilePos, Tile)]
type InventoryChanges = [InventoryChange]

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
