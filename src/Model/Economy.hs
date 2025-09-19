{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.Economy where

import GHC.Generics (Generic)
import Data.Maybe (fromMaybe)
import Data.Foldable (find)
import Text.Printf (printf)
import Data.List (intercalate)
import Data.Text (Text)



newtype Port = Port Text
  deriving (Show, Eq)


-- Основная структура корабля
data Ship = Ship
  { shipType      :: ShipType
  , shipCapacity      :: Int            -- максимальная грузоподъёмность
  , shipCrew          :: Int            -- количество моряков
  , shipSpeed         :: Double         -- узлы (или абстрактная величина)
  , shipHullIntegrity :: HullIntegrity  -- состояние корпуса
  , shipCargo         :: [CargoItem]    -- текущий груз
  , shipStatus        :: ShipStatus     -- статус
  } deriving (Show, Eq)

data ShipType = Cog | Holk | Crayer | Kraier | Hulk | Caravel
  deriving (Show, Eq, Generic)

data ShipStatus = AtSea | InPort Port | UnderRepair
  deriving (Show, Eq)


data Goods = Grain | Beer | Cloth | Salt | Iron | Fish | Honey | Wine
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

type HullIntegrity = Int

type Price = Int

-- Количество единиц товара на рынке
type Stock = Int


-- Элемент груза: товар + количество
data CargoItem = CargoItem
  { cargoItemGoods :: Goods
  , cargoItemQuantity :: Int  -- количество единиц
  } deriving (Show, Eq)


-- Элемент рынка: товар + цена + запас
data MarketItem = MarketItem
  { marketItemGoods :: Goods
  , marketItemPrice       :: Price
  , marketItemStock       :: Stock
  } deriving (Show, Eq)


-- Рынок города — список позиций
newtype Market = Market [MarketItem]
  deriving (Show, Eq)


-- Город: название, рынок, специализация (что он производит)
data City = City
  { cityPort      :: Port
  , cityMarket    :: Market
  , cityProduction    :: [(Goods, Int)] -- производство: товар + сколько производится за тик
  , cityConsumption   :: [(Goods, Int)] -- потребление: товар + сколько тратится за тик
  } deriving (Show, Eq)


-- Торговая сделка
data TradeAction
  = Buy  Goods Int  -- купить N единиц товара
  | Sell Goods Int  -- продать N единиц товара
  deriving (Show, Eq)

-- Результат сделки (успех или ошибка)
data TradeResult
  = TradeSuccess City Market
  | TradeError String
  deriving (Show, Eq)

-- ============================================================== --

-- Найти производство/потребление конкретного товара
lookupAmount :: Goods -> [(Goods, Int)] -> Int
lookupAmount g xs = fromMaybe 0 (lookup g xs)


-- Обновление одного MarketItem за один ход
updateMarketItem :: [(Goods, Int)] -> [(Goods, Int)] -> MarketItem -> MarketItem
updateMarketItem prod cons item =
  let produced = lookupAmount (marketItemGoods item) prod
      consumed = lookupAmount (marketItemGoods item) cons
      newStock = max 0 (marketItemStock item + produced - consumed)
      basePrice = 100
      -- Простейшая модель ценообразования: чем меньше запасов, тем дороже
      newPrice = basePrice + (if newStock == 0 then 100 else 0)
               + (50 - min 50 newStock)  -- наценка при низком запасе
  in item { marketItemStock = newStock, marketItemPrice = newPrice }


-- Обновляем рынок целиком
updateMarket :: [(Goods, Int)] -> [(Goods, Int)] -> Market -> Market
updateMarket prod cons (Market items) =
  Market (map (updateMarketItem prod cons) items)


-- Обновляем город за один ход
simulateCityTick :: City -> City
simulateCityTick city =
  city { cityMarket = updateMarket (cityProduction city) (cityConsumption city) (cityMarket city) }


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
removeFromCargo g q = filter (\c -> not (cargoItemGoods c == g && cargoItemQuantity c <= q))

-- Купить товар
buyGoods :: Ship -> City -> Goods -> Int -> Either String (Ship, City)
buyGoods ship city g q = case findMarketItem g (cityMarket city) of
  Nothing -> Left "Market item is not found in market"
  Just item ->
    if marketItemStock item < q
      then Left "Not enough goods in market"
      else if q > (shipCapacity ship - sum (map cargoItemQuantity (shipCargo ship)))
        then Left "Not enough capacity at ship"
        else
          let newItem   = item { marketItemStock = marketItemStock item - q }
              newMarket = updateMarketItemInMarket newItem (cityMarket city)
              newCargo  = addToCargo g q (shipCargo ship)
          in Right (ship { shipCargo = newCargo }, city { cityMarket = newMarket })

-- Продать товар
sellGoods :: Ship -> City -> Goods -> Int -> Either String (Ship, City)
sellGoods ship city g q =
  case find (\c -> cargoItemGoods c == g && cargoItemQuantity c >= q) (shipCargo ship) of
    Nothing -> Left "There are no such cargo at ship"
    Just _  ->
      case findMarketItem g (cityMarket city) of
        Nothing -> Left "There are no such cargo at city market"
        Just item ->
          let newItem   = item { marketItemStock = marketItemStock item + q }
              newMarket = updateMarketItemInMarket newItem (cityMarket city)
              newCargo  = removeFromCargo g q (shipCargo ship)
          in Right (ship { shipCargo = newCargo }, city { cityMarket = newMarket })

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
performRouteStep :: Ship -> City -> RouteStep -> Either String (Ship, City)
performRouteStep ship city (RouteStep port actions) =
  let shipInPort = sailTo ship port
  in foldl (\acc action ->
       case acc of
         Left err -> Left err
         Right (s, c) -> case action of
           Buy g q  -> buyGoods s c g q
           Sell g q -> sellGoods s c g q
     ) (Right (shipInPort, city)) actions


-- | Выполнить один полный проход маршрута для корабля
-- Принимает: корабль, список всех городов, маршрут
-- Возвращает: обновлённый корабль и обновлённый список городов
simulateTradeRouteTick :: Ship -> [City] -> TradeRoute -> Either String (Ship, [City])
simulateTradeRouteTick ship cities (TradeRoute steps) =
  go ship cities steps
  where
    go s cs [] = Right (s, cs) -- маршрут пройден
    go s cs (RouteStep port acts : rest) =
      case find (\c -> cityPort c == port) cs of
        Nothing -> Left $ "Port " ++ show port ++ " is not found"
        Just city ->
          case performRouteStep s city (RouteStep port acts) of
            Left err -> Left err
            Right (s', updatedCity) ->
              let cs' = updatedCity : filter ((/= cityPort updatedCity) . cityPort) cs
              in go s' cs' rest


-- | Выполняем несколько тиков симуляции для заданного маршрута.
simulateTradeRouteNTicks :: Int -> Ship -> [City] -> TradeRoute -> Either String (Ship, [City])
simulateTradeRouteNTicks n ship cities route = go n ship cities
  where
    go 0 s cs = Right (s, cs)
    go k s cs =
      let cs' = simulateEconomyTick cs  -- экономика развивается каждый тик
      in case simulateTradeRouteTick s cs' route of
           Left err -> Left $ "Error on step " ++ show (n - k + 1) ++ ": " ++ err
           Right (s', cs'') -> go (k - 1) s' cs''


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
    [ "🚢 Ship (" ++ show (shipType ship) ++ ")"
    , "  Location: " ++ case shipStatus ship of
        AtSea       -> "At sea"
        UnderRepair -> "Under repair"
        InPort p    -> "In port " ++ show p
    , printf "  Capacity: %d | Crew: %d | Speed: %.1f"
        (shipCapacity ship) (shipCrew ship) (shipSpeed ship)
    , "  Hull integrity: " ++ show (shipHullIntegrity ship) ++ "%"
    , "  Cargo: " ++ prettyCargo (shipCargo ship)
    ]

-- | Pretty print market
prettyMarket :: Market -> String
prettyMarket (Market items) =
  intercalate "\n"
    [ printf "    %-6s | Price: %3d | Stock: %3d"
        (show (marketItemGoods i))
        (marketItemPrice i)
        (marketItemStock i)
    | i <- items ]

-- | Pretty print city state
prettyCity :: City -> String
prettyCity city =
  unlines
    [ "🏙 City: " ++ show (cityPort city)
    , "  Market:"
    , prettyMarket (cityMarket city)
    , "  Production: " ++ show (cityProduction city)
    , "  Consumption: " ++ show (cityConsumption city)
    ]

-- | Pretty print all cities
prettyCities :: [City] -> String
prettyCities = intercalate "\n\n" . map prettyCity

-- ============================================================== --
