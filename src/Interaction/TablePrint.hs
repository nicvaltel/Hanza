{-# LANGUAGE OverloadedStrings #-}

module Interaction.TablePrint where

import Colonnade (headed, ascii)
import qualified Data.Text as T
import Text.Printf ( printf )

import Model.Types


-- | Table view for ship cargo
tableCargo :: [CargoItem] -> T.Text
tableCargo items =
  if null items
    then "Empty\n"
    else  T.pack $ ascii cargoColumns items
  where
    cargoColumns =
      mconcat
        [ headed "Goods"   (show . cargoItemGoods)
        , headed "Quantity" ( show . cargoItemQuantity)
        ]

-- | Table view for ship state
tableShip :: Ship -> T.Text
tableShip ship =
  "=== Ship ===\n" <>
  T.pack (ascii shipColumns [ship]) <>
  "Cargo:\n" <>
  tableCargo (shipCargo ship) <>
  "\n"
  where
    shipColumns =
      mconcat
        [ headed "Type"        (show . shipType)
        , headed "Location"    (prettyLocation . shipStatus)
        , headed "Capacity"    (show . shipCapacity)
        , headed "Crew"        (show . shipCrew)
        , headed "Speed"       (printf "%.1f" . shipSpeed)
        , headed "Hull"        ((++ "%") . show . shipHullIntegrity)
        ]

    prettyLocation st = case st of
      AtSea       -> "At sea"
      UnderRepair -> "Under repair"
      InPort p    -> "In port " ++ show p

-- | Table view for city market
tableMarket :: Market -> T.Text
tableMarket (Market items) = T.pack $ ascii marketColumns items
  where
    marketColumns =
      mconcat
        [ headed "Goods"  (show . marketItemGoods)
        , headed "Price"  (show . marketItemPrice)
        , headed "Stock"  (show . marketItemStock)
        ]

-- | Table view for city state
tableCity :: City -> T.Text
tableCity city =
   "=== City: " <> T.pack (show $ cityPort city) <> " ===\n" <>
   tableMarket (cityMarket city) <>
   "Production: "  <> T.pack (show $ cityProduction city) <> "\n" <>
   "Consumption: " <> T.pack (show $ cityConsumption city) <> "\n" <>
    "\n"

-- | Table view for multiple cities
tableCities :: [City] -> T.Text
tableCities = T.concat . map tableCity
