{-# LANGUAGE TemplateHaskell #-}
module Calendar where

import Control.Lens.TH

data Calendar = Calendar 
    { _dayView :: Bool
    , _currentYear :: Integer
    , _currentMonth :: Int
    , _currentDay :: Int
    , _focusedDay :: Int
    , _day :: Day
    } deriving (Show)

data Event = Event Int

data Day = Day 
    { _focusedHour :: Int 
    } deriving (Show)

makeLenses ''Calendar
makeLenses ''Day

