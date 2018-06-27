{-# LANGUAGE OverloadedStrings #-}
module Main where

import Brick
import Control.Lens
import qualified Data.Time as Time
import qualified Data.Time.Calendar.MonthDay as MonthDay
import Graphics.Vty

data Calendar = Calendar 
    { _currentYear :: Integer
    , _currentMonth :: Int
    , _currentDay :: Int 
    } deriving (Show)

--data Month = Month 
--    { _name :: String
--    , _currentDays :: [Day]
--    } deriving (Show)

--data Day = Day 
--    { _hours :: [Hour]
--    , _date :: Int
--    } deriving (Show)
-- TODO: days will have dates

--data Hour = One deriving (Show) 
-- TODO: hours will have name fields
-- TODO: hours will have events/schedules

--makeLenses ''Calendar
--makeLenses ''Month
--makeLenses ''Day

--allMonths :: Traversal' Calendar Month
--allMonths = months . traversed

--datesOfMonth :: Traversal' Month Int 
--datesOfMonth = days . traversed . date

--monthNames :: Traversal' [Month] String
--monthNames = traversed . name

getToday :: IO (Integer, Int, Int)
getToday = Time.getCurrentTime >>= return . Time.toGregorian . Time.utctDay

padRightWithSpaces :: Int -> Widget a -> Widget a
padRightWithSpaces n = (padRight (Pad n))

dateToWidget :: Calendar -> String -> Widget a
dateToWidget c d
  | length d == 1 && (read d :: Int) == _currentDay c = styleToday . (padRightWithSpaces 3) . str $ d
  | length d == 1 = (padRightWithSpaces 3) . str $ d
  | length d == 2 && (read d :: Int) == _currentDay c = styleToday . (padRightWithSpaces 2) . str $ d
  | otherwise = (padRightWithSpaces 2) . str $ d

styleToday :: Widget a -> Widget a
styleToday = withAttr (attrName "whitebg")

datesUI :: Calendar -> [[Int]] -> Widget a 
datesUI c dates = vBox $ widgetsToLines $ ((<$>) . (<$>)) ((dateToWidget c) . show) dates

widgetsToLines :: [[Widget a]] -> [Widget a]
widgetsToLines w = foldr (<+>) emptyWidget <$> w

splitAtAll :: Int -> [Int] -> [[Int]]
splitAtAll _ [] = []
splitAtAll c xs = [(fst $ splitAt c xs)] ++ splitAtAll c (drop c xs)

getDaysInMonth :: Calendar -> [Int]
getDaysInMonth c = [1..(MonthDay.monthLength (Time.isLeapYear $ _currentYear c) (_currentMonth c))]

ui :: Calendar -> Widget a
ui c = do  
    str "June" 
    <=> datesUI c (splitAtAll 7 (getDaysInMonth c))

drawUI :: Calendar -> [Widget a]
drawUI c = return $ ui c

type A = ()
type B = ()

handleEvent :: Calendar -> BrickEvent a b -> EventM c (Next Calendar)
handleEvent c (VtyEvent (EvKey KEsc [])) = halt c
handleEvent c (VtyEvent (EvKey (KChar 'q') [])) = halt c
handleEvent c _ = continue c

attributeMap :: AttrMap
attributeMap = attrMap defAttr [("whitebg", bg white)]

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

