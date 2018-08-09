{-# LANGUAGE TemplateHaskell #-}
module Calendar where

import Brick.Widgets.Edit
import Control.Lens.TH

data Calendar = Calendar 
    { _currentView :: View
    , _currentYear :: Integer
    , _currentMonth :: Int
    , _currentDay :: Int
    , _focusedDay :: Int
    , _day :: Day
    } deriving (Show)

data Day = Day 
    { _currentHour :: Int
    , _hourInfo :: [HourInfo]
    } deriving (Show)

data HourInfo = HourInfo
    { _name :: String
    , _description :: String
    , _editor :: Editor String String
    , _time :: Int
    } deriving (Show)

data View = MonthView | DayView | EditView deriving (Show)

makeLenses ''Calendar
makeLenses ''Day
makeLenses ''HourInfo

