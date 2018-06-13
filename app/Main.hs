{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Brick
import Control.Lens
import Graphics.Vty

data Calendar = Calendar
    { _days :: [Day]
    , _hours :: [Hour]
    }

data Day = Day Int
data Hour = Hour Int

makeLenses ''Calendar

calendar :: Calendar
calendar = Calendar { _days = [Day 10]
                    , _hours = [Hour 10]
                    }

ui :: Widget a
ui = str "Hello, world!" <=> str "My name is Chris..."

drawUI :: Calendar -> [Widget a]
drawUI c = [ui]

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
main = defaultMain app calendar

