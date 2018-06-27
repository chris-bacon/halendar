module Calendar where

data Calendar = Calendar 
    { _currentYear :: Integer
    , _currentMonth :: Int
    , _currentDay :: Int 
    } deriving (Show)

