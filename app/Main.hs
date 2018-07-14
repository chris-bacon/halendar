{-# LANGUAGE OverloadedStrings #-}
module Main where

import Brick
import qualified Brick.Widgets.Edit as Edit
import qualified Data.Time as Time
import Graphics.Vty

import Calendar
import UI
import Events

getToday :: IO (Integer, Int, Int)
getToday = Time.getCurrentTime >>= return . Time.toGregorian . Time.utctDay

attributeMap :: AttrMap
attributeMap = attrMap defAttr 
    [("focusedDay", (black `on` white))
    , (Edit.editAttr, white `on` blue)
    , (Edit.editFocusedAttr, blue `on` white)
    ]

type A = ()

app :: App Calendar A String
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
        _currentView = MonthView
        , _currentYear = year
        , _currentMonth = month
        , _currentDay = date
        , _focusedDay = date
        , _day = Day 5 [Event 1 "Test Name" "Test description"]
        , _editor = Edit.editor "Editor" Nothing "Hello"
        }
    defaultMain app calendar

