{-# LANGUAGE OverloadedStrings #-}


module Interaction.TablePrint where




import Colonnade (headed, ascii)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Model.Economy
import Text.Printf ( printf )

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
tableShip :: Ship -> IO ()
tableShip ship = do
  putStrLn "=== Ship ==="
  T.putStr $ T.pack $ ascii shipColumns [ship]
  putStrLn "Cargo:"
  T.putStr $ tableCargo (shipCargo ship)
  putStrLn "\n"
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
tableCity :: City -> IO ()
tableCity city = do
  putStrLn $ "=== City: " ++ show (cityPort city) ++ " ==="
  T.putStr $ tableMarket (cityMarket city)
  putStrLn $ "Production: "  ++ show (cityProduction city)
  putStrLn $ "Consumption: " ++ show (cityConsumption city)
  putStrLn "\n"

-- | Table view for multiple cities
tableCities :: [City] -> IO ()
tableCities = mapM_ tableCity
