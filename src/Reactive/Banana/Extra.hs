module Reactive.Banana.Extra where

import Reactive.Banana


filterEq :: Eq a => a -> Event t a -> Event t ()
filterEq x = fmap (const ()) . filterE (== x)
