{-# LANGUAGE TemplateHaskell #-}
module Calendar where

import Control.Lens.TH

data Calendar = Calendar 
    { _dayView :: Bool
    , currentYear :: Integer
    , currentMonth :: Int
    , currentDay :: Int
    , _focusedDay :: Int
    , day :: Day
    } deriving (Show)

data Event = Event Int

data Day = Day 
    { focusedHour :: Int 
    } deriving (Show)

makeLenses ''Calendar

