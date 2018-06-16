{-# LANGUAGE OverloadedStrings #-}
module Main where

import Brick
import Control.Lens
import qualified Data.Time as Time
import qualified Data.Time.Calendar.MonthDay as MonthDay
import Graphics.Vty

data Calendar = Calendar { _months :: [Month] } deriving (Show)

data Month = Month 
    { _name :: String
    , _days :: [Day]
    } deriving (Show)

data Day = Day 
    { _hours :: [Hour]
    , _date :: Int
    } deriving (Show)
-- TODO: days will have dates

data Hour = One deriving (Show) 
-- TODO: hours will have name fields
-- TODO: hours will have events/schedules

--makeLenses ''Calendar
--makeLenses ''Month
--makeLenses ''Day

getToday :: IO (Integer, Int, Int)
getToday = Time.getCurrentTime >>= return . Time.toGregorian . Time.utctDay

c1 :: Calendar
c1 = Calendar 
    { _months = 
        [ Month 
            { _name = "Febuary"
            , _days = 
                [ Day 
                    { _hours = [One]
                    , _date = 1
                    }
                , Day 
                    { _hours = [One]
                    , _date = 2
                    }
                ]
            }
        , Month 
            { _name = "June"
            , _days = 
                [ Day 
                    { _hours = [One]
                    , _date = 1
                    }
                , Day 
                    { _hours = [One]
                    , _date = 2
                    }
                ]
            }
        ] 
    }

--allMonths :: Traversal' Calendar Month
--allMonths = months . traversed

--datesOfMonth :: Traversal' Month Int 
--datesOfMonth = days . traversed . date

dateToWidget :: String -> Widget a
dateToWidget d = 
    if length d == 1 
       then ((padRight (Pad 3)) . str $ d)
       else ((padRight (Pad 2)) . str $ d)

datesUI :: [[Int]] -> Widget a 
datesUI dates = vBox $ widgetsToLines $ ((<$>) . (<$>)) (dateToWidget . show) dates

widgetsToLines :: [[Widget a]] -> [Widget a]
widgetsToLines w = foldr (<+>) emptyWidget <$> w

--monthNames :: Traversal' [Month] String
--monthNames = traversed . name

splitAtAll :: Int -> [Int] -> [[Int]]
splitAtAll _ [] = []
splitAtAll c xs = [(fst $ splitAt c xs)] ++ splitAtAll c (drop c xs)

ui :: Calendar -> Widget a
ui c = do
    str "June" <=> datesUI (splitAtAll 7 [1..(MonthDay.monthLength False 6)])
--        where
--            month = c ^.. allMonths
--            dates = (head month) ^.. datesOfMonth

drawUI :: Calendar -> [Widget a]
drawUI c = return $ ui c

type A = ()
type B = ()

handleEvent :: Calendar -> BrickEvent a b -> EventM c (Next Calendar)
handleEvent c (VtyEvent (EvKey KEsc [])) = halt c
handleEvent c _ = continue c

attributeMap :: AttrMap
attributeMap = attrMap defAttr [("bg", red `on` red)]

app :: App Calendar A B
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const attributeMap
          }

main :: IO Calendar
main = defaultMain app c1

