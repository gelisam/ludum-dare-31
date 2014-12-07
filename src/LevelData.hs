module LevelData where

import Types
import Vec2d


gameTitle :: String
gameTitle = "More of the Same"


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

initialStage :: [[Tile]]
initialStage = [[Start, Floor, Floor, Floor, Wall ]
               ,[Wall , Floor, Floor, Floor, Wall ]
               ,[Wall , Floor, Floor, Floor, Wall ]
               ,[Wall , Floor, Floor, Floor, Wall ]
               ,[Wall , Floor, Floor, Floor, Goal]]

levelData :: [LevelChanges]
levelData = [[(V 2 3, Wall)]
            ,[(V 2 4, LockedDoor)]
            ,[(V 2 1, Key 0)]
            ,[(V 1 4, LockedDoor)]
            ,[(V 3 3, Wall)]
            ]

lastLevel :: LevelNumber
lastLevel = length levelData
