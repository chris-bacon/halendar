module Calendar where

data Calendar = Calendar 
    { dayView :: Bool
    , currentYear :: Integer
    , currentMonth :: Int
    , currentDay :: Int
    , focusedDay :: Int
    } deriving (Show)

