{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.Gloss.Extra where

import Control.Applicative
import Data.Array
import Data.Monoid
import Graphics.Gloss

import Vec2d


pictureRow :: Float -> [Picture] -> Picture
pictureRow _ [] = blank
pictureRow dx (x:xs) = x <> translate dx 0 (pictureRow dx xs)

pictureCol :: Float -> [Picture] -> Picture
pictureCol _ [] = blank
pictureCol dy (x:xs) = x <> translate 0 dy (pictureCol dy xs)


insideGrid :: Float -> Float -> Array (V Int) a -> Picture -> Picture
insideGrid cellSizeX cellSizeY cells = translate (-centerX) (-centerY)
  where
    (_, V w h) = bounds cells
    dimV = V (fromIntegral w) (fromIntegral h)
    
    cellSizeV = V cellSizeX cellSizeY
    totalSizeV = cellSizeV * dimV
    V centerX centerY = totalSizeV / 2

atGridPos :: Float -> Float -> V Float -> Picture -> Picture
atGridPos dx dy (V x y) = translate (x * dx) (y * dy)

pictureGrid :: Float -> Float -> Array (V Int) Picture -> Picture
pictureGrid dx dy cells = insideGrid dx dy cells
                        $ pictures
                        $ cellPicture <$> indices cells
  where
    cellPicture :: V Int -> Picture
    cellPicture v = translate (x * dx) (y * dy)
                  $ cells ! v
      where
        x, y :: Float
        x = fromIntegral (vx v)
        y = fromIntegral (vy v)


guardPicture :: Bool -> Picture -> Picture
guardPicture True = id
guardPicture False = const blank


rectBoldPicture :: V Int -> Picture -> Picture
rectBoldPicture size picture = pictures $ offsetPicture <$> range (0, size)
  where
    offsetPicture (V x y) =  translate (fromIntegral x) (fromIntegral y) picture

boldPicture :: Picture -> Picture
boldPicture = rectBoldPicture (V 1 0)
