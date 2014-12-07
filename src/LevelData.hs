module LevelData where

import Data.Array
import Data.List

import Types
import Vec2d


gameTitle :: String
gameTitle = "I've Seen This Room Before"


startTilePos :: TilePos
startTilePos = 0

goalTilePos :: TilePos
goalTilePos = 4

startScreenPos :: ScreenPos
startScreenPos = fmap fromIntegral startTilePos

goalScreenPos :: ScreenPos
goalScreenPos = fmap fromIntegral goalTilePos

initialPlayerGraphics :: PlayerGraphics
initialPlayerGraphics = PlayerGraphics True startScreenPos

initialStage :: Stage
initialStage = listArray (0, maxIndex)
             $ concat
             $ transpose smallRoom
  where
    smallRoom :: [[Tile]]
    smallRoom = [[Start, Floor, Floor, Floor, Wall]
                ,[Wall , Floor, Floor, Floor, Wall]
                ,[Wall , Floor, Floor, Floor, Wall]
                ,[Wall , Floor, Floor, Floor, Wall]
                ,[Wall , Floor, Floor, Floor, Goal]]
    
    w, h :: Int
    w = length (head smallRoom)
    h = length smallRoom
    
    maxIndex :: TilePos
    maxIndex = V (w-1) (h-1)

levelData :: [LevelChanges]
levelData = [[(V 2 3, Wall)]
            ,[(V 2 4, LockedDoor)]
            ,[(V 2 1, Key 0)]
            ,[(V 1 4, LockedDoor)]
            ,[(V 3 3, Wall)]
            ]

lastLevel :: LevelNumber
lastLevel = length levelData
