module Graphics where

import Control.Applicative
import Data.Monoid
import Graphics.Gloss

import Vec2d


pictureRow :: Float -> [Picture] -> Picture
pictureRow dx [] = blank
pictureRow dx (x:xs) = x <> translate dx 0 (pictureRow dx xs)

pictureCol :: Float -> [Picture] -> Picture
pictureCol dy [] = blank
pictureCol dy (x:xs) = x <> translate 0 dy (pictureCol dy xs)


grid :: Float -> Float -> [[Picture]] -> Picture
grid _ _ [] = blank
grid cellSizeX cellSizeY cells = translate (-centerX) (-centerY)
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
