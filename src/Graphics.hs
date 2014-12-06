module Graphics where

import Control.Applicative
import Data.Monoid
import Graphics.Gloss

import Vec2d


pictureRow :: Float -> [Picture] -> Picture
pictureRow _ [] = blank
pictureRow dx (x:xs) = x <> translate dx 0 (pictureRow dx xs)

pictureCol :: Float -> [Picture] -> Picture
pictureCol _ [] = blank
pictureCol dy (x:xs) = x <> translate 0 dy (pictureCol dy xs)


pictureGrid :: Float -> Float -> [[Picture]] -> Picture
pictureGrid _ _ [] = blank
pictureGrid cellSizeX cellSizeY cells = translate (-centerX) (-centerY)
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
