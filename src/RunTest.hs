{-# LANGUAGE OverloadedStrings #-}

module RunTest where
import Model.Economy
import TablePrint (tableShip, tableCities)





runTest :: IO ()
runTest = do
  let lubeckMarket = Market [MarketItem Grain 100 30, MarketItem Beer 100 10]
  let lubeck = City (Port "Lubeck") lubeckMarket [(Grain, 5)] [(Beer, 3)]
  putStrLn "lubeck:"
  print lubeck
  let lubeck1 = head $ simulateEconomyTick [lubeck]
  putStrLn "simulateEconomyTick [lubeck]:"
  print lubeck1

  let myShip = Ship Cog 100 10 8.0 100 [] (InPort (Port "Lubeck"))
  putStrLn "myShip:"
  print myShip

  -- Покупаем 20 зерна
  case buyGoods myShip lubeck1 Grain 20 of
    Left err -> putStrLn err
    Right (ship1, city1) -> do
      putStrLn "myShip:"
      print ship1
      putStrLn "lubeck:"
      print city1

      -- Перевозим корабль в Гамбург
      let ship2 = sailTo ship1 (Port "Hamburg")
      putStrLn "myShip:"
      print ship2

  pure ()


runTest2 :: IO ()
runTest2 = do
  let portHamburg = Port "Hamburg"
  let portLubeck = Port "Lubeck"
  let lubeck = City portLubeck (Market [MarketItem Grain 100 50]) [(Grain, 5)] []
  let hamburg = City portHamburg (Market [MarketItem Grain 100 10]) [] [(Grain, 3)]
  let myShip = Ship Cog 100 10 8.0 100 [] (InPort portLubeck)

  let route = TradeRoute
        [ RouteStep portLubeck  [Buy Grain 20]
        , RouteStep portHamburg [Sell Grain 20]
        ]

  case simulateTradeRouteTick myShip [lubeck, hamburg] route of
    Left err -> putStrLn err
    Right (shipAfter, updatedCities) -> do
      print shipAfter
      print updatedCities


runTest3 :: IO ()
runTest3 = do
  let portHamburg = Port "Hamburg"
  let portLubeck = Port "Lubeck"
  let lubeck = City portLubeck (Market [MarketItem Grain 100 5000]) [(Grain, 5)] []
  let hamburg = City portHamburg (Market [MarketItem Grain 100 1000]) [] [(Grain, 3)]
  let myShip = Ship Cog 100 10 8.0 100 [] (InPort portLubeck)

  let route = TradeRoute
        [ RouteStep portLubeck  [Buy Grain 20]
        , RouteStep portHamburg [Sell Grain 20]
        ]

  -- case simulateTradeRouteNTicks 10 myShip [lubeck, hamburg] route of
  --   Left err -> putStrLn err
  --   Right (finalShip, finalCities) -> do
  --     print finalShip
  --     print finalCities
  
  case simulateTradeRouteNTicks 5 myShip [lubeck, hamburg] route of
    Left err -> putStrLn err
    Right (shipAfter, updatedCities) -> do
      putStrLn $ prettyShip shipAfter
      putStrLn ""
      putStrLn $ prettyCities updatedCities


runTest4 :: IO ()
runTest4 = do
  putStrLn "\n"

  let portHamburg = Port "Hamburg"
  let portLubeck = Port "Lubeck"
  let lubeck = City portLubeck (Market [MarketItem Grain 100 5000]) [(Grain, 5)] []
  let hamburg = City portHamburg (Market [MarketItem Grain 100 1000]) [] [(Grain, 3)]
  let myShip = Ship Cog 100 10 8.0 100 [] (InPort portLubeck)

  let route = TradeRoute
        [ RouteStep portLubeck  [Buy Grain 20]
        , RouteStep portHamburg [Sell Grain 20]
        ]

 
  case simulateTradeRouteNTicks 5 myShip [lubeck, hamburg] route of
    Left err -> putStrLn err
    Right (shipAfter, updatedCities) -> do
      tableShip shipAfter
      tableCities updatedCities
