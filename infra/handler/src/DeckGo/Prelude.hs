module DeckGo.Prelude where

import Data.Function

xif :: b -> ((b -> c) -> b -> c) -> c
xif = flip fix
