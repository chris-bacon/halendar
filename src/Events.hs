module Events where

import Brick
import Graphics.Vty

import Calendar

handleEvent :: Calendar -> BrickEvent a b -> EventM c (Next Calendar)
handleEvent c (VtyEvent (EvKey KEsc [])) = halt c
handleEvent c (VtyEvent (EvKey (KChar 'q') [])) = halt c
handleEvent c _ = continue c

