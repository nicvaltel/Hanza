module Model.ErrorMsgs where



data ErrorMsg =
  ErrMarketItemIsNotFoundInMarket
  deriving (Eq)


instance Show ErrorMsg where
  show ErrMarketItemIsNotFoundInMarket = "Market item is not found in market"
