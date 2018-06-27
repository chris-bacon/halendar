{-# LANGUAGE OverloadedStrings #-}
module Main where

import Brick
import Control.Lens
import qualified Data.Time as Time

import Calendar
import UI
import Events

getToday :: IO (Integer, Int, Int)
getToday = Time.getCurrentTime >>= return . Time.toGregorian . Time.utctDay

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
    (year, month, day) <- getToday
    let calendar = Calendar { _currentYear = year, _currentMonth = month, _currentDay = day }
    defaultMain app calendar

