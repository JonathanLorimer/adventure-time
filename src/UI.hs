module UI where

import Brick
import Edit (Action(..))
import qualified Graphics.Vty as V

data Mode = PickMode
          | Play
          | Edit Action

type Resource = ()

ui :: App Mode e Resource
ui = App { appDraw         = drawUI
         , appChooseCursor = showFirstCursor
         , appHandleEvent  = undefined
         , appStartEvent   = return
         , appAttrMap      = const customAttrMap
         }

drawUI :: Mode -> [Widget Resource]
drawUI = undefined

customAttrMap :: AttrMap
customAttrMap = undefined

handleEvent :: Mode -> BrickEvent Resource e -> EventM Resource (Next Mode)
handleEvent g (VtyEvent (V.EvKey V.KUp []))    = continue g
handleEvent g (VtyEvent (V.EvKey V.KDown []))  = continue g
handleEvent g (VtyEvent (V.EvKey V.KEnter [])) = continue g
handleEvent g _                                = continue g
