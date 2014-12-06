module Graphics where

import Control.Applicative
import Data.Monoid
import Graphics.Gloss

import Vec2d
import Debug.Trace


pictureRow :: Float -> [Picture] -> Picture
pictureRow dx [] = blank
pictureRow dx (x:xs) = x <> translate dx 0 (pictureRow dx xs)

pictureCol :: Float -> [Picture] -> Picture
pictureCol dy [] = blank
pictureCol dy (x:xs) = x <> translate 0 dy (pictureCol dy xs)


grid :: Float -> Float -> [[Picture]] -> Picture
grid _ _ [] = blank
grid cellSizeX cellSizeY cells = traceShow (cellSizeV, dimV, totalSizeV, centerX, centerY) $ translate (-centerX) (-centerY)
                               $ pictureCol cellSizeY
                               $ pictureRow cellSizeX
                             <$> cells
  where
    w = length (head cells)
    h = length cells
    dimV = V (fromIntegral w) (fromIntegral h)
    
    cellSizeV = V cellSizeX cellSizeY
    totalSizeV = cellSizeV * dimV
    V centerX centerY = totalSizeV / 2


letterPicture :: String -> Picture
letterPicture s = translate (-5) (-6)
                $ scale 0.12 0.12
                $ text s

startPicture :: Picture
startPicture = circle 10
            <> letterPicture "S"

goalPicture :: Picture
goalPicture = circle 10
           <> letterPicture "G"

playerPicture :: Picture
playerPicture = circle 10
             <> rotate 90 (letterPicture ":)")

lockedDoorPicture :: Picture
lockedDoorPicture = circle 10
                 <> rotate (-90) (letterPicture "D")
                 <> rotate (-90) (letterPicture "-")
