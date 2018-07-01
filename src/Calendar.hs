module Calendar where

data Calendar = Calendar 
    { currentYear :: Integer
    , currentMonth :: Int
    , currentDay :: Int
    , focusedDay :: Int
    } deriving (Show)

