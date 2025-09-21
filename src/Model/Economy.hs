{-# LANGUAGE OverloadedStrings #-}

module Model.Economy where

import Data.Maybe (fromMaybe)
import Data.Foldable (find, Foldable (foldl'))
import Text.Printf (printf)
import Data.List (intercalate)

import Model.Types


-- Найти производство/потребление конкретного товара
lookupAmount :: Goods -> [(Goods, Int)] -> Int
lookupAmount g xs = fromMaybe 0 (lookup g xs)


-- Обновляем рынок целиком
updateMarket :: City -> City
updateMarket city =
  let newMarket =
        updateMarketByProduction (cityProductionSchemas city) $
        updateMarketByConsumption (cityConsumption city) (cityMarket city)
  in city{cityMarket = newMarket}


-- Обновляем рынок с учетом потребления
updateMarketByConsumption :: [(Goods, Int)] -> Market -> Market
updateMarketByConsumption cons (Market items) =
  Market (map updateItemByConsumption items)
  where
    updateItemByConsumption :: MarketItem -> MarketItem
    updateItemByConsumption item =
      let consumed = lookupAmount (marketItemGoods item) cons
          newStock = max 0 (marketItemStock item - consumed)
          basePrice = 100 :: Int
          -- Простейшая модель ценообразования: чем меньше запасов, тем дороже
          newPrice = basePrice + (if newStock == 0 then 100 else 0)
                  + (50 - min 50 newStock)  -- наценка при низком запасе
      in item { 
                marketItemStock = newStock
              , marketItemPriceAsk = Price newPrice
              , marketItemPriceBid = Price (ceiling (fromIntegral newPrice * 0.9 :: Double)) 
              }

-- Обновляем рынок с учетом производства
updateMarketByProduction :: [ProductionSchema] -> Market -> Market
updateMarketByProduction schemas (Market startItems) = Market (foldl' (flip produceOneProduct) startItems schemas)
  where
    produceOneProduct :: ProductionSchema -> [MarketItem] -> [MarketItem]
    produceOneProduct (ProductionSchema ptProduct ptQuantity ptIngridients) items = do
      let productionFunc maybeAcc itm = 
            case maybeAcc of
              Nothing -> Nothing
              Just acc -> 
                if marketItemGoods itm == ptProduct
                  then Just (itm{marketItemStock = marketItemStock itm + ptQuantity} : acc) -- добавили произведенный продукт к marketItems
                  else
                    case lookup (marketItemGoods itm) ptIngridients of
                      Just q
                        | marketItemStock itm >= q -> Just (itm{marketItemStock = marketItemStock itm - q} : acc)
                        | otherwise -> Nothing -- Если не хватило количества сырья, то этот продукт не производится
                      Nothing -> Just(itm:acc) -- = это не продукт и не ингридиент, ничего не меняем

      let maybeNewItems = reverse <$> foldl' productionFunc (Just []) items
      -- если maybeNewItems == Nothing, значит не хватило количества сырья, продукт не производится, [MarketItem] не изменился
      -- если maybeNewItems == Just newItems, значит хватило количества сырья, продукт производится, [MarketItem] изменился
      fromMaybe items maybeNewItems




-- Обновляем город за один ход
simulateCityTick :: City -> City
simulateCityTick = updateMarket


-- Обновляем список городов (вся экономика за один тик)
simulateEconomyTick :: [City] -> [City]
simulateEconomyTick = map simulateCityTick


-- Найти MarketItem по товару
findMarketItem :: Goods -> Market -> Maybe MarketItem
findMarketItem g (Market items) = find ((== g) . marketItemGoods) items

-- Обновить MarketItem внутри Market
updateMarketItemInMarket :: MarketItem -> Market -> Market
updateMarketItemInMarket updated (Market items) =
  Market (updated : filter ((/= marketItemGoods updated) . marketItemGoods) items)

-- Добавить товар в груз корабля
addToCargo :: Goods -> Int -> [CargoItem] -> [CargoItem]
addToCargo g q [] = [CargoItem g q]
addToCargo g q (c:cs)
  | cargoItemGoods c == g = CargoItem g (cargoItemQuantity c + q) : cs
  | otherwise    = c : addToCargo g q cs

-- Убрать товар из груза корабля
removeFromCargo :: Goods -> Int -> [CargoItem] -> [CargoItem]
removeFromCargo g q = 
  filter (\ci -> cargoItemQuantity ci > 0) .
  map (\ci -> if cargoItemGoods ci == g then ci{cargoItemQuantity = cargoItemQuantity ci - q} else ci )

getBuyCost :: MarketItem -> Quantity -> Money
getBuyCost item q = unPrice (marketItemPriceAsk item) * q

getSellCost :: MarketItem -> Quantity -> Money
getSellCost item q = unPrice (marketItemPriceBid item) * q
  
-- Купить товар
buyGoods :: Money -> Ship -> City -> Goods -> Int -> Either String (Money, Ship, City)
buyGoods money ship city g q = case findMarketItem g (cityMarket city) of
  Nothing -> Left "Market item is not found in market"
  Just item ->
    if marketItemStock item < q
      then Left "Not enough goods in market"
      else if q > (shipCapacity ship - sum (map cargoItemQuantity (shipCargo ship)))
        then Left "Not enough capacity at ship"
        else if money >= getBuyCost item q
          then
            let newItem   = item { marketItemStock = marketItemStock item - q }
                newMarket = updateMarketItemInMarket newItem (cityMarket city)
                newCargo  = addToCargo g q (shipCargo ship)
            in Right (money - getBuyCost item q, ship { shipCargo = newCargo }, city { cityMarket = newMarket })
          else Left "Not enought money"

-- Продать товар
sellGoods :: Money -> Ship -> City -> Goods -> Int -> Either String (Money, Ship, City)
sellGoods money ship city g q =
  case find (\c -> cargoItemGoods c == g && cargoItemQuantity c >= q) (shipCargo ship) of
    Nothing -> Left "There are no such cargo at ship"
    Just _  ->
      case findMarketItem g (cityMarket city) of
        Nothing -> Left "There are no such cargo at city market"
        Just item ->
          let newItem   = item { marketItemStock = marketItemStock item + q }
              newMarket = updateMarketItemInMarket newItem (cityMarket city)
              newCargo  = removeFromCargo g q (shipCargo ship)
          in Right (money + getSellCost item q , ship { shipCargo = newCargo }, city { cityMarket = newMarket })

-- Перевести корабль в другой город
sailTo :: Ship -> Port -> Ship
sailTo ship newCity = ship { shipStatus = InPort newCity }


-- | Описание одной точки маршрута
data RouteStep = RouteStep
  { stepPort      :: Port           -- порт, куда идём
  , stepActions   :: [TradeAction]  -- список торговых действий
  } deriving (Show, Eq)

-- | Маршрут — цикл из шагов
newtype TradeRoute = TradeRoute [RouteStep]
  deriving (Show, Eq)

-- | Перемещаем корабль в порт и выполняем торговлю
performRouteStep :: Money -> Ship -> City -> RouteStep -> Either String (Money, Ship, City)
performRouteStep money ship city (RouteStep port actions) =
  let shipInPort = sailTo ship port
  in foldl (\acc action ->
       case acc of
         Left err -> Left err
         Right (m ,s, c) -> case action of
           Buy g q  -> buyGoods m s c g q
           Sell g q -> sellGoods m s c g q
     ) (Right (money, shipInPort, city)) actions


-- | Выполнить один полный проход маршрута для корабля
-- Принимает: корабль, список всех городов, маршрут
-- Возвращает: обновлённый корабль и обновлённый список городов
simulateTradeRouteTick :: Money -> Ship -> [City] -> TradeRoute -> Either String (Money, Ship, [City])
simulateTradeRouteTick money ship cities (TradeRoute steps) =
  go money ship cities steps
  where
    go m s cs [] = Right (m, s, cs) -- маршрут пройден
    go m s cs (RouteStep port acts : rest) =
      case find (\c -> cityPort c == port) cs of
        Nothing -> Left $ "Port " ++ show port ++ " is not found"
        Just city ->
          case performRouteStep m s city (RouteStep port acts) of
            Left err -> Left err
            Right (m', s', updatedCity) ->
              let cs' = updatedCity : filter ((/= cityPort updatedCity) . cityPort) cs
              in go m' s' cs' rest


-- | Выполняем несколько тиков симуляции для заданного маршрута.
simulateTradeRouteNTicks :: Int -> Money -> Ship -> [City] -> TradeRoute -> Either String (Money, Ship, [City])
simulateTradeRouteNTicks n money ship cities route = go money n ship cities
  where
    go m 0 s cs = Right (m, s, cs)
    go m k s cs =
      let cs' = simulateEconomyTick cs  -- экономика развивается каждый тик
      in case simulateTradeRouteTick m s cs' route of
           Left err -> Left $ "Error on step " ++ show (n - k + 1) ++ ": " ++ err
           Right (m', s', cs'') -> go m' (k - 1) s' cs''


-- | Pretty print ship cargo
prettyCargo :: [CargoItem] -> String
prettyCargo [] = "Empty"
prettyCargo items =
  intercalate ", "
    [ show (cargoItemGoods c) ++ ": " ++ show (cargoItemQuantity c)
    | c <- items ]

-- | Pretty print ship state
prettyShip :: Ship -> String
prettyShip ship =
  unlines
    [ "Ship (" ++ show (shipType ship) ++ ")"
    , "  Location: " ++ case shipStatus ship of
        AtSea       -> "At sea"
        UnderRepair -> "Under repair"
        InPort p    -> "In port " ++ show p
    , printf "  Capacity: %d | Crew: %d | Speed: %.1f"
        (shipCapacity ship) (shipCrew ship) (shipSpeed ship)
    , "  Hull integrity: " ++ show (shipHullIntegrity ship) ++ "%"
    , "  Cargo: " ++ prettyCargo (shipCargo ship)
    ]

prettyMoney :: Money -> String
prettyMoney money = "Money: " <> show money <> "$"

-- | Pretty print market
prettyMarket :: Market -> String
prettyMarket (Market items) =
  intercalate "\n"
    [ printf "    %-6s | Ask: %3d | Bid: %3d | Stock: %3d"
        (show (marketItemGoods i))
        (unPrice $ marketItemPriceAsk i)
        (unPrice $ marketItemPriceBid i)
        (marketItemStock i)
    | i <- items ]

-- | Pretty print city state
prettyCity :: City -> String
prettyCity city =
  unlines
    [ "City: " ++ show (cityPort city)
    , "  Market:"
    , prettyMarket (cityMarket city)
    -- , "  Production: " ++ show (cityProduction city)
    , "  Consumption: " ++ show (cityConsumption city)
    ]

-- | Pretty print all cities
prettyCities :: [City] -> String
prettyCities = intercalate "\n\n" . map prettyCity

-- ============================================================== --
