module Model.Types where


newtype Port = Port String
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
  deriving (Show, Eq)

data ShipStatus = AtSea | InPort Port | UnderRepair
  deriving (Show, Eq)


data Goods = Grain | Beer | Cloth | Salt | Iron | Fish | Honey | Wine
  deriving (Show, Eq, Ord, Enum, Bounded)

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


-- | Глобальное состояние игры
data GameState = GameState
  { gameShip  :: Ship
  , gameCities :: [City]
  } deriving (Show)
