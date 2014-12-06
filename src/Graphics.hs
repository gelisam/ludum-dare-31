{-# LANGUAGE RecordWildCards #-}
module Graphics where

import Data.Monoid
import Graphics.Gloss
import Text.Printf

import Graphics.Gloss.Extra
import Sprites
import Types


insideStage :: Stage -> Picture -> Picture
insideStage = insideGrid 20 20

renderStage :: Stage -> Picture
renderStage = pictureGrid 20 20
            . (fmap.fmap) tilePicture

renderHUD :: LevelNumber -> Picture
renderHUD = translate (-315) (200)
          . scale 0.2 0.2
          . text
          . printf "Level %d"

renderGameState :: GameState -> Picture
renderGameState (GameState {..}) = renderHUD levelNumber
                                <> renderStage stage
                                <> insideStage stage playerPicture
