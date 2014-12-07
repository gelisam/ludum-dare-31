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
          . blackText
          . printf "Level %d"


insideStage :: Stage -> Picture -> Picture
insideStage = insideGrid 60 60

renderStage :: Stage -> Picture
renderStage = pictureGrid 60 60
            . fmap tilePicture

atStagePos :: V Float -> Picture -> Picture
atStagePos = atGridPos 60 60


renderPlayer :: PlayerGraphics -> Picture
renderPlayer (PlayerGraphics {..}) = guardPicture gPlayerVisible
                                   $ atStagePos gPlayerScreenPos playerPicture


renderInventory :: Inventory -> Picture
renderInventory = translate (-300) 180
                . pictureRow 35
                . fmap (scale 0.5 0.5 . const keyPicture)


renderDebugMessage :: String -> Picture
renderDebugMessage = translate (-315) (-235)
                   . scale 0.1 0.1
                   . blackText

renderDebugMessages :: [String] -> Picture
renderDebugMessages = pictureCol 11 . fmap renderDebugMessage


renderGameState :: GameState -> Picture
renderGameState (GameState {..}) = renderHUD gLevelNumber
                                <> renderStage gStage
                                <> insideStage gStage (renderPlayer gPlayerGraphics)
                                <> renderInventory gInventory
                                <> renderDebugMessages gDebugMessages
