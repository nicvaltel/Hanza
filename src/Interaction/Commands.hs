{-# LANGUAGE OverloadedStrings #-}

module Interaction.Commands where

import Data.Foldable (find)
import Model.Economy
import Model.Types



buy :: Goods -> Int -> GameState -> Either (String, City) GameState
buy g q (GameState ship cities) =
  case shipStatus ship of
    InPort port ->
      case find (\c -> cityPort c == port) cities of
        Nothing -> Left ("Current port not found: " ++ show port, dummyCity)
        Just city ->
          case buyGoods ship city g q of
            Left err -> Left (err, city)
            Right (ship', city') ->
              let newCities = city' : filter ((/= cityPort city) . cityPort) cities
              in Right (GameState ship' newCities)
    _ -> Left ("Ship is not in port", dummyCity)
  where
    dummyCity = head cities -- fallback, just to have a City in Left


sell :: Goods -> Int -> GameState -> Either (String, Ship) GameState
sell g q (GameState ship cities) =
  case shipStatus ship of
    InPort port ->
      case find (\c -> cityPort c == port) cities of
        Nothing -> Left ("Current port not found: " ++ show port, ship)
        Just city ->
          case sellGoods ship city g q of
            Left err -> Left (err, ship)
            Right (ship', city') ->
              let newCities = city' : filter ((/= cityPort city) . cityPort) cities
              in Right (GameState ship' newCities)
    _ -> Left ("Ship is not in port", ship)


goto :: Port -> GameState -> Either (String, Ship) GameState
goto newPort gameSt = do
  let ship = gameShip gameSt
  case shipStatus ship of
    InPort port | port == newPort -> Left ("Ship is already in " <> show newPort, ship)
    _ -> Right (gameSt{gameShip = ship{shipStatus = InPort newPort}})