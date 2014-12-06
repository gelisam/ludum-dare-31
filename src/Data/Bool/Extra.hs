module Data.Bool.Extra where


if_then_else :: Bool -> a -> a -> a
if_then_else True  t _ = t
if_then_else False _ f = f
