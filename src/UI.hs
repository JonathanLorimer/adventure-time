module UI where

-- External Imports
import Brick
import qualified Data.Map as M
import Data.Maybe
import qualified Graphics.Vty as V

-- Internal Imports
import Types
import Menu
import Play (drawPlay)
import Edit
import UIHelpers

ui :: App AppState e Resource
ui = App { appDraw         = drawUI
         , appChooseCursor = showFirstCursor
         , appHandleEvent  = handleEvent
         , appStartEvent   = return
         , appAttrMap      = const customAttrMap
         }

drawUI :: AppState -> [Widget Resource]
drawUI AppState { mode = PickStory, stories, cursor} = uiBase PickStory $ drawStories cursor (M.elems stories)
drawUI AppState {mode = PickMode, cursor} = uiBase PickMode $ drawPickMode cursor
drawUI a@AppState {mode = Play} = uiBase Play [drawPlay a]
drawUI AppState {mode = (Edit Nothing), cursor} = uiBase (Edit Nothing) (drawPickAction cursor)
drawUI AppState {mode = (Edit (Just a)), story = (Just s)} = uiBase (Edit Nothing) [drawEdit a s]
drawUI _ = error "something went wrong"

customAttrMap :: AttrMap
customAttrMap = attrMap V.defAttr []

handleEvent :: AppState -> BrickEvent Resource e -> EventM Resource (Next AppState)
handleEvent s (VtyEvent (V.EvKey V.KUp []))    =
  if cursor s == 0
    then continue s
    else continue $ s { cursor = cursor s - 1 }
handleEvent s (VtyEvent (V.EvKey V.KDown []))  =
  case mode s of
   PickStory -> safeMove (M.size (stories s) - 1)
   PickMode -> safeMove 1
   Play     -> safeMove $ ((+ (-1)) . length . choices) (fromJust $ curPassage s)
   _ -> continue s
  where
    safeMove i = if cursor s == i
                  then continue s
                  else continue $ s { cursor = cursor s + 1 }

handleEvent s (VtyEvent (V.EvKey V.KEnter [])) = continue $ transitionState s
handleEvent s (VtyEvent (V.EvKey V.KEsc []))   = halt s
handleEvent s _                                = continue s

transitionState :: AppState -> AppState
transitionState s =
  case mode s of
    -- Menu Transitions
    PickStory -> s { mode = PickMode
                   , story = Just $ M.elems (stories s) !! cursor s
                   , cursor = 0
                   }
    PickMode  -> s { mode = if cursor s == 0
                               then Edit Nothing
                               else Play
                   , curPassage = story s >>=
                      \st -> M.lookup (start st) (passages st)
                   }

    -- Play Transitions
    Play  -> case choices $ fromJust (curPassage s) of
                [] -> s { mode       = PickStory
                        , story      = Nothing
                        , curPassage = Nothing
                        , cursor     = 0
                        }
                xs -> s { curPassage =
                       M.lookup (xs !! cursor s)
                                (fromJust $ passages <$> story s)
                        }

    -- Edit Transitions
    Edit Nothing                       -> s
    Edit (Just AddPassage)             -> s
    Edit (Just RemovePassage)          -> s
    Edit (Just (EditPassage Nothing))  -> s
    Edit (Just (EditPassage (Just p))) -> s

