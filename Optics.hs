-- https://impurepics.com/posts/2020-03-22-optics.html

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE TemplateHaskell            #-}

module Optics
    ( stocks
    , beerNames
    ) where

import           Control.Lens
import           Data.Monoid

newtype Name = Name String deriving (Show, Semigroup, Monoid)
newtype Stock = Stock (Sum Int) deriving (Show, Semigroup, Monoid, Num)

data Bar = Bar
    { _barFridge :: Maybe Fridge
    }
    deriving Show
data Fridge = Fridge
    { _fridgeBeers :: [Beer]
    }
    deriving Show
data Beer = Beer
    { _beerName  :: Maybe Name
    , _beerStock :: Maybe Stock
    }
    deriving Show

makeLenses ''Bar
makeLenses ''Fridge
makeLenses ''Beer

stocks = traverse.barFridge._Just.fridgeBeers.traverse.beerStock._Just
beerNames = traverse.barFridge._Just.fridgeBeers.traverse.beerName._Just

main :: IO ()
main = do
    let bars = [ (Bar Nothing), (Bar (
          Just $ Fridge [
            (Beer (Just $ Name "Corona Extra") (Just $ Stock (Sum 4)))
          , (Beer (Just $ Name "IPO Beer") (Just $ Stock (Sum 3)))
          ]))]
    print $ view stocks bars
    print $ set stocks 10 bars
    print $ over stocks (+1) bars

    print $ view beerNames bars
    print $ set beerNames (Name "New name") bars
    print $ over beerNames (<> (Name "!")) bars
