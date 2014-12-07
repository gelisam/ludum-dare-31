module Types where

import Vec2d


gameTitle :: String
gameTitle = "More of the Same"


type LevelNumber = Int

data Tile = Start
          | Goal
          | Floor
          | Wall
          | LockedDoor
          | UnlockedDoor
          | Key LevelNumber
  deriving (Show, Eq)

type Stage = [[Tile]]

type LevelChanges = [(V Int, Tile)]

type TilePos = V Int
type ScreenPos = V Float

data PlayerGraphics = PlayerGraphics
  { gPlayerVisible      :: Bool
  , gPlayerScreenPos    :: ScreenPos
  } deriving (Show, Eq)

data GameState = GameState
  { gLevelNumber        :: LevelNumber
  , gStage              :: Stage
  , gPlayerTilePos      :: TilePos
  , gPlayerGraphics     :: PlayerGraphics
  , gAccumulatedChanges :: [LevelChanges]
  , gDebugMessages      :: [String]
  } deriving (Show, Eq)
