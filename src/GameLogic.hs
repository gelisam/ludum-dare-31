module GameLogic where

import Types
import Vec2d


validateTilePos :: Stage -> TilePos -> Maybe Tile
validateTilePos = atV

validateTile :: Inventory -> Tile -> Maybe InventoryChanges
validateTile _     Wall       = Nothing
validateTile []    LockedDoor = Nothing
validateTile (k:_) LockedDoor = Just [ConsumeKey k]
validateTile _     (Key k)    = Just [ReceiveKey k]
validateTile _     _          = Just []

validateMove :: Stage -> Inventory -> TilePos -> Maybe Move
validateMove stage ks tilePos = do
    tile <- validateTilePos stage tilePos
    inventoryChanges <- validateTile ks tile
    return $ Move tile
                  tilePos
                  (fmap fromIntegral tilePos)
                  inventoryChanges
