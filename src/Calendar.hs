{-# LANGUAGE TemplateHaskell #-}
module Calendar where

import Control.Lens.TH

data Calendar = Calendar 
    { _currentView :: View
    , _currentYear :: Integer
    , _currentMonth :: Int
    , _currentDay :: Int
    , _focusedDay :: Int
    , _day :: Day
    } deriving (Show)

data Event = Event
    { _time :: Int
    , _name :: String
    , _description :: String
    } deriving (Show)

data Day = Day 
    { _focusedHour :: Int
    , _events :: [Event]
    } deriving (Show)

data View = MonthView | DayView deriving (Show)

makeLenses ''Calendar
makeLenses ''Day
makeLenses ''Event

