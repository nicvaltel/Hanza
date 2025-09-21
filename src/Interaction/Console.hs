{-# LANGUAGE OverloadedStrings #-}

module Interaction.Console where

import qualified Interaction.Commands  as Commands
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
          case Commands.buy g qty (appStateGame appSt) of
            Left (err, city) -> putStrLn $ "Error: " ++ err ++ "\n" ++ prettyCity city
            Right newGameState   -> do
              putStrLn "Purchase successful"
              writeIORef ref appSt{appStateGame = newGameState}
        _ -> putStrLn "Usage: buy <goods> <quantity>"

    ["sell", goodsStr, qtyStr] ->
      case (parseGoods goodsStr, reads qtyStr :: [(Int, String)]) of
        (Just g, [(qty, "")]) -> do
          appSt <- readIORef ref
          case Commands.sell g qty (appStateGame appSt) of
            Left (err, ship) -> putStrLn $ "Error: " ++ err ++ "\n" ++ prettyShip ship
            Right newGameState   -> do
              putStrLn "Sell successful"
              writeIORef ref appSt{appStateGame = newGameState}
        _ -> putStrLn "Usage: buy <goods> <quantity>"
    
    ["goto", portStr] ->
      case parsePort portStr of
        Just port -> do
          appSt <- readIORef ref
          case Commands.goto port (appStateGame appSt) of 
            Left (err, ship) -> putStrLn $ "Error: " ++ err ++ "\n" ++ prettyShip ship
            Right newGameState-> do
              putStrLn $ "Successfully sail to " <> show port
              writeIORef ref appSt{appStateGame = newGameState} 
        _ -> putStrLn "Usage: goto <lubeck|hamburg>"

    ["tick"] -> tick
    ["t"] -> tick

    ["quit"] -> quit
    ["q"] -> quit
    [":q"] -> quit

    _ -> putStrLn $ "Unknown command. Available:\n" <> "Type 'status' to view current state, 'buy grain 10' to buy, 'quit' to exit, 'help' for this info."

    where
      status = do
        appSt <- readIORef ref
        let (GameState ship cities money) = appStateGame appSt
        case appStateDisplayFormat appSt of
          DisplayFormatText -> do
            putStrLn (prettyMoney money)
            putStrLn (prettyShip ship)
            putStrLn (prettyCities cities)
          DisplayFormatTable -> do
            putStrLn (prettyMoney money)
            T.putStrLn $ tableShip ship
            T.putStrLn $ tableCities cities

      tick = do
        appSt <- readIORef ref
        let gs = appStateGame appSt
        writeIORef ref appSt{appStateGame = Commands.tick gs}
        status

      quit = do
        putStrLn "Goodbye!"
        -- завершаем программу
        ioError (userError "exit")


-- | Главная функция: простой цикл
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  ref <- newIORef initialState

  putStrLn "Welcome to The Patrician 2 Economy Simulator!"
  putStrLn helpMsg

  let loop = do
        putStr "> "
        cmd <- getLine
        handleCommand ref cmd
        loop
  loop



-- Инициализация тестового состояния
port1 = Port "Lubeck"
port2 = Port "Hamburg"
productionSchemas1 = [ProductionSchema{ptProduct = Beer, ptQuantity = 3, ptIngridients = [(Grain,4)]}] :: [ProductionSchema]
productionSchemas2 = [ProductionSchema{ptProduct = Grain, ptQuantity = 5, ptIngridients = []}] :: [ProductionSchema]
city1 = City port1 (Market [MarketItem Grain (Price 100) (Price 90) 50, MarketItem Beer (Price 120) (Price 100) 20]) [] productionSchemas1
city2 = City port2 (Market [MarketItem Grain (Price 90) (Price 80) 30, MarketItem Beer (Price 200) (Price 180) 0]) [(Grain, 3)] productionSchemas2
ship0 = Ship Cog 100 10 8.0 100 [] (InPort port1)
initialState = AppState{appStateGame = GameState ship0 [city1, city2] 300, appStateDisplayFormat = DisplayFormatTable}

simulations = T.putStrLn . tableCities . take 5 $ iterate simulateCityTick city2
