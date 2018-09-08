module Events where

import Brick
import Brick.Types
import qualified Brick.Widgets.Edit as E
import Control.Lens
import Graphics.Vty

import Calendar

-- TODO: Remove magic numbers!
-- TODO: Handle ends and beginning of month
handleEvent :: Calendar -> BrickEvent a b -> EventM String (Next Calendar)
handleEvent c (VtyEvent (EvKey KEsc [])) = halt c
handleEvent c@(Calendar DayView _ _ _ _ _) (VtyEvent (EvKey (KChar 'w') [])) = continue $ c & (day . currentHour) -~ 1
handleEvent c@(Calendar DayView _ _ _ _ _) (VtyEvent (EvKey (KChar 's') [])) = continue $ c & (day . currentHour) +~ 1
handleEvent c@(Calendar DayView _ _ _ _ _) (VtyEvent (EvKey (KChar 'q') [])) = continue $ c & currentView .~ MonthView
handleEvent c@(Calendar DayView _ _ _ _ _) (VtyEvent (EvKey KEnter [])) = continue $ c & currentView .~ EditView
handleEvent c@(Calendar EditView _ _ _ _ _) (VtyEvent (EvKey KEnter [])) = continue $ c & currentView .~ DayView
handleEvent c@(Calendar EditView _ _ _ _ day) (VtyEvent event) = undefined -- continue =<< 
    --handleEventLensed 
    --c 
    --((a day) . editor)
    --E.handleEditorEvent 
    --event
handleEvent c (VtyEvent (EvKey (KChar 'r') [])) = continue $ c & focusedDay .~ c ^. currentDay
handleEvent c (VtyEvent (EvKey (KChar 'w') [])) = continue $ c & focusedDay -~ 7
handleEvent c (VtyEvent (EvKey (KChar 'a') [])) = continue $ c & focusedDay -~ 1
handleEvent c (VtyEvent (EvKey (KChar 's') [])) = continue $ c & focusedDay +~ 7
handleEvent c (VtyEvent (EvKey (KChar 'd') [])) = continue $ c & focusedDay +~ 1
handleEvent c (VtyEvent (EvKey KEnter [])) = continue $ c & currentView .~ DayView
handleEvent c (VtyEvent (EvKey (KChar 'q') [])) = halt c
handleEvent c _ = continue c


a :: Day -> HourInfo
a day = getCurrentHourInfo (day ^. currentHour) (day ^. hourInfo)

getCurrentHourInfo :: Int -> [HourInfo] -> HourInfo
getCurrentHourInfo n (h:hs) = if n == (h ^. time) then h else getCurrentHourInfo n hs

