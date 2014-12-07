module LevelData where

import Types


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
initialStage = [[Start, Floor, Floor, Floor, Floor]
               ,[Floor, Floor, Floor, Floor, Floor]
               ,[Floor, Floor, Floor, Floor, Floor]
               ,[Floor, Floor, Floor, Floor, Floor]
               ,[Floor, Floor, Floor, Floor, Goal]]
