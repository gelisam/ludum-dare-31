module GameLogic where

import Data.Array
import Data.List

import Types


validateTilePos :: Stage -> TilePos -> Maybe Tile
validateTilePos xs i | inRange (bounds xs) i = Just (xs ! i)
                     | otherwise             = Nothing

validateTile :: Inventory -> Tile -> Maybe InventoryChanges
validateTile _     Wall       = Nothing
validateTile _     XWall      = Nothing
validateTile []    LockedDoor = Nothing
validateTile (k:_) LockedDoor = Just [ConsumeKey k]
validateTile _     (Key k)    = Just [ReceiveKey k]
validateTile _     _          = Just []

validateMove :: Stage -> Inventory -> TilePos -> Maybe Move
validateMove stage ks tilePos = do
    tile <- validateTilePos stage tilePos
    inventoryChanges <- validateTile ks tile
    return $ Move (consumeTile tile)
                  tilePos
                  (fmap fromIntegral tilePos)
                  inventoryChanges

consumeTile :: Tile -> Tile
consumeTile LockedDoor = UnlockedDoor
consumeTile (Key _)    = Floor
consumeTile tile       = tile

changeTile :: (TilePos, Tile) -> Stage -> Stage
changeTile change = (// [change])

changeStage :: LevelChanges -> Stage -> Stage
changeStage = flip (//)

changeInventory :: InventoryChanges -> Inventory -> Inventory
changeInventory = foldr (.) id
                . fmap go
  where
    go :: InventoryChange -> Inventory -> Inventory
    go (ReceiveKey k) = (k:)
    go (ConsumeKey k) = delete k
