{-# LANGUAGE OverloadedStrings #-}
module Main where

import Brick
import qualified Data.Time as Time
import Graphics.Vty

import Calendar
import UI
import Events

getToday :: IO (Integer, Int, Int)
getToday = Time.getCurrentTime >>= return . Time.toGregorian . Time.utctDay

attributeMap :: AttrMap
attributeMap = attrMap defAttr [("focusedDay", (black `on` white))]

type A = ()
type B = ()

app :: App Calendar A B
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const attributeMap
          }

main :: IO Calendar
main = do
    (year, month, date) <- getToday
    let calendar = Calendar {
        _dayView = False
        , _currentYear = year
        , _currentMonth = month
        , _currentDay = date
        , _focusedDay = date
        , _day = Day 5
        }
    defaultMain app calendar

