{-# LANGUAGE RecordWildCards #-}
module Graphics where

import Data.Monoid
import Graphics.Gloss
import Text.Printf

import Graphics.Gloss.Extra
import Sprites
import Types
import Vec2d


renderHUD :: LevelNumber -> Picture
renderHUD = translate (-315) 200
          . scale 0.2 0.2
          . text
          . printf "Level %d"


insideStage :: Stage -> Picture -> Picture
insideStage = insideGrid 20 20

renderStage :: Stage -> Picture
renderStage = pictureGrid 20 20
            . (fmap.fmap) tilePicture

atStagePos :: V Int -> Picture -> Picture
atStagePos = atGridPos 20 20


renderPlayer :: Player -> Picture
renderPlayer pos = atStagePos pos playerPicture


renderDebugMessage :: String -> Picture
renderDebugMessage = translate (-315) (-235)
                   . scale 0.1 0.1
                   . text

renderDebugMessages :: [String] -> Picture
renderDebugMessages = pictureCol 11 . fmap renderDebugMessage


renderGameState :: GameState -> Picture
renderGameState (GameState {..}) = renderHUD gLevelNumber
                                <> renderStage gStage
                                <> insideStage gStage (renderPlayer gPlayer)
                                <> renderDebugMessages gDebugMessages
