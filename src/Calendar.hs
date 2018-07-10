{-# LANGUAGE TemplateHaskell #-}
module Calendar where

import Control.Lens.TH

data Calendar = Calendar 
    { dayView :: Bool
    , currentYear :: Integer
    , currentMonth :: Int
    , currentDay :: Int
    , focusedDay :: Int
    } deriving (Show)

data Event = Event Int

data Day = Day Int

makeLenses ''Calendar

