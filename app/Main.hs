{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Brick
import Control.Lens
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

makeLenses ''Calendar
makeLenses ''Month
makeLenses ''Day

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

getMonths :: Traversal' Calendar Month
getMonths = months . traversed

datesOfMonth :: Traversal' Month Int 
datesOfMonth = days . traversed . date

datesUI :: [Int] -> Widget a
datesUI dates = hBox (fmap (padRight (Pad 5)) (fmap str (fmap show dates)))

ui :: Calendar -> Widget a
ui c = do
    str monthName <=> datesUI dates
        where
            month = c ^.. getMonths
            dates = (head month) ^.. datesOfMonth
            monthName = (head month) ^. name

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

