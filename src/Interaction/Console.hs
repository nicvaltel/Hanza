{-# LANGUAGE OverloadedStrings #-}

module Interaction.Console where

import Interaction.Commands
import Data.IORef
import System.IO
import Data.Char (toLower)
import Interaction.TablePrint (tableShip, tableCities)
import qualified Data.Text.IO as T

import Model.Economy
import Model.Types

data DisplayFormat = DisplayFormatText | DisplayFormatTable
  deriving (Eq, Ord, Show)

switchDisplayFormat :: DisplayFormat -> DisplayFormat
switchDisplayFormat DisplayFormatTable = DisplayFormatText
switchDisplayFormat DisplayFormatText = DisplayFormatTable

data AppState = AppState {
    appStateGame :: GameState
  , appStateDisplayFormat :: DisplayFormat
}


welcomeMsg, helpMsg :: String
welcomeMsg = "Welcome to Hanza's Wind Economy Simulator!"
helpMsg = "Type 'help' for this info, 'switch' to switch display format, 'status' to view current state, 'buy grain 10' to buy, 'goto hamburg' to sail, 'quit' to exit." :: String

-- | Парсим имя товара в конструктор Goods
parseGoods :: String -> Maybe Goods
parseGoods str =
  case map toLower str of
    "grain" -> Just Grain
    "beer"  -> Just Beer
    "cloth" -> Just Cloth
    "salt"  -> Just Salt
    "iron"  -> Just Iron
    "fish"  -> Just Fish
    "honey" -> Just Honey
    "wine"  -> Just Wine
    _       -> Nothing

parsePort :: String -> Maybe Port
parsePort str =
  case map toLower str of
    "lubeck" -> Just $ Port "Lubeck"
    "hamburg" -> Just $ Port "Hamburg"
    _ -> Nothing

-- | Обрабатываем одну команду пользователя
handleCommand :: IORef AppState -> String -> IO ()
handleCommand ref input =
  case map toLower <$> words input of
    ["help"] -> do
      putStrLn helpMsg

    ["switch"] -> do
      appSt <- readIORef ref
      let df = appStateDisplayFormat appSt
      let newDf = switchDisplayFormat df
      putStrLn $ "Switch to " <> show newDf
      writeIORef ref appSt{appStateDisplayFormat = newDf}

    ["status"] -> status
    ["s"] -> status

    ["buy", goodsStr, qtyStr] ->
      case (parseGoods goodsStr, reads qtyStr :: [(Int, String)]) of
        (Just g, [(qty, "")]) -> do
          appSt <- readIORef ref
          case buy g qty (appStateGame appSt) of
            Left (err, city) -> putStrLn $ "Error: " ++ err ++ "\n" ++ prettyCity city
            Right newGameState   -> do
              putStrLn "Purchase successful"
              writeIORef ref appSt{appStateGame = newGameState}
        _ -> putStrLn "Usage: buy <goods> <quantity>"

    ["sell", goodsStr, qtyStr] ->
      case (parseGoods goodsStr, reads qtyStr :: [(Int, String)]) of
        (Just g, [(qty, "")]) -> do
          appSt <- readIORef ref
          case sell g qty (appStateGame appSt) of
            Left (err, ship) -> putStrLn $ "Error: " ++ err ++ "\n" ++ prettyShip ship
            Right newGameState   -> do
              putStrLn "Sell successful"
              writeIORef ref appSt{appStateGame = newGameState}
        _ -> putStrLn "Usage: buy <goods> <quantity>"
    
    ["goto", portStr] ->
      case parsePort portStr of
        Just port -> do
          appSt <- readIORef ref
          case goto port (appStateGame appSt) of 
            Left (err, ship) -> putStrLn $ "Error: " ++ err ++ "\n" ++ prettyShip ship
            Right newGameState-> do
              putStrLn $ "Successfully sail to " <> show port
              writeIORef ref appSt{appStateGame = newGameState} 
        _ -> putStrLn "Usage: goto <lubeck|hamburg>"


    ["quit"] -> quit
    ["q"] -> quit
    [":q"] -> quit

    _ -> putStrLn $ "Unknown command. Available:\n" <> "Type 'status' to view current state, 'buy grain 10' to buy, 'quit' to exit, 'help' for this info."

    where
      status = do
        appSt <- readIORef ref
        let (GameState ship cities) = appStateGame appSt
        case appStateDisplayFormat appSt of
          DisplayFormatText -> do
            putStrLn (prettyShip ship)
            putStrLn (prettyCities cities)
          DisplayFormatTable -> do
            T.putStrLn $ tableShip ship
            T.putStrLn $ tableCities cities

      quit = do
        putStrLn "Goodbye!"
        -- завершаем программу
        ioError (userError "exit")


-- | Главная функция: простой цикл
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  -- Инициализация тестового состояния
  let port1 = Port "Lubeck"
      port2 = Port "Hamburg"

      city1 = City port1 (Market [MarketItem Grain 100 50, MarketItem Beer 120 20]) [(Grain, 5)] []
      city2 = City port2 (Market [MarketItem Grain 90 30, MarketItem Beer 200 0]) [] [(Grain, 3)]

      ship0 = Ship Cog 100 10 8.0 100 [] (InPort port1)
      initialState = AppState{appStateGame = GameState ship0 [city1, city2], appStateDisplayFormat = DisplayFormatTable}

  ref <- newIORef initialState

  putStrLn "Welcome to The Patrician 2 Economy Simulator!"
  putStrLn helpMsg

  let loop = do
        putStr "> "
        cmd <- getLine
        handleCommand ref cmd
        loop
  loop
