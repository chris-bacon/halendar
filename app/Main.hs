{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Brick
import Control.Lens
import Graphics.Vty

data Calendar = Calendar { _months :: [Month] } deriving (Show)

data Month = Month { _month :: Febuary }--January | Febuary | March | April | May | June | July | August | September | October | November | December 
    deriving (Show)

data Febuary = Febuary { _days :: [Day] } deriving (Show)

data Day = Day { _hours :: [Hour] } deriving (Show)
-- TODO: days will have dates

data Hour = One deriving (Show) 
-- TODO: hours will have name fields
-- TODO: hours will have events/schedules

makeLenses ''Calendar
makeLenses ''Month
makeLenses ''Febuary
makeLenses ''Day

c1 :: Calendar
c1 = Calendar { _months = [Month (Febuary { _days = [Day {_hours = [One]}]})] }

ui :: Calendar -> Widget a
ui c = undefined
    --let m = c ^. months
  --in str $ (head ((head m) ^. month days)) ^. hours ._1 -- <=> 
       --str $ m ^. 

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

