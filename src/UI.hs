module UI where

import Brick
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import Edit (Action(..))
import Data.Map (Map)
import qualified Data.List as L
import qualified Data.Map as M
import Types
import qualified Graphics.Vty as V

data Mode = PickStory
          | PickMode
          | Play
          | Edit (Maybe Action)
          deriving (Eq, Ord, Show)

data AppState = AppState { mode    :: Mode
                         , stories :: Map Title Story
                         , story   :: Maybe Story
                         , cursor  :: CursorPos }

type Resource = ()
type CursorPos = Int

ui :: App AppState e Resource
ui = App { appDraw         = drawUI
         , appChooseCursor = showFirstCursor
         , appHandleEvent  = handleEvent
         , appStartEvent   = return
         , appAttrMap      = const customAttrMap
         }

drawUI :: AppState -> [Widget Resource]
drawUI (AppState PickStory       s _ c) = drawBorder PickStory $ drawStories c (M.elems s)
drawUI (AppState PickMode        _ _ c) = drawBorder PickMode $ drawPickMode c
drawUI (AppState Play            _ (Just s) c) = drawBorder Play [drawPlay s]
drawUI (AppState (Edit Nothing)  _ _ c) = drawBorder (Edit Nothing) [drawPickAction]
drawUI (AppState (Edit (Just a)) _ (Just s) c) = drawBorder (Edit Nothing) [drawEdit a s]
drawUI _ = error "something went wrong"

customAttrMap :: AttrMap
customAttrMap = attrMap V.defAttr []

handleEvent :: AppState -> BrickEvent Resource e -> EventM Resource (Next AppState)
handleEvent s (VtyEvent (V.EvKey V.KUp []))    =
  if cursor s == 0
    then continue s
    else continue $ s { cursor = cursor s - 1 }
handleEvent s (VtyEvent (V.EvKey V.KDown []))  =
  if cursor s == (M.size (stories s) - 1)
    then continue s
    else continue $ s { cursor = cursor s + 1 }
handleEvent s (VtyEvent (V.EvKey V.KEnter [])) = continue $ transitionState s
handleEvent s (VtyEvent (V.EvKey V.KEsc []))   = halt s
handleEvent s _                                = continue s

transitionState :: AppState -> AppState
transitionState s =
  case mode s of
    PickStory -> s { mode = PickMode
                   , story = Just $ M.elems (stories s) !! cursor s
                   , cursor = 0 }
    _ -> s
{-
    PickMode  ->
    Play      ->
    Edit _    ->
-}

drawBorder :: Mode -> [Widget Resource] -> [Widget Resource]
drawBorder m w = [ C.center
  $ withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str $ "Adventure Time - Mode: " <> show m)
  $ C.center
  $ vBox w ]

drawStories :: CursorPos -> [Story] -> [Widget Resource]
drawStories _ [] = [txtWrap . ("➤ " <>) $ "create story"]
drawStories p ss = txtWrap <$> (fst . prefix $ showTitle . storyTitle <$> ss)
  where
    prefix l = foldr
      (\e (xs, i) -> if i == p
                      then ("➤ " <> e : xs, i - 1)
                      else ("  " <> e : xs, i - 1))
      ([], L.length l - 1) l

drawPickMode :: CursorPos -> [Widget Resource]
drawPickMode 0 = [txtWrap "➤ Edit Mode", txtWrap "  Play Mode"]
drawPickMode 1 = [txtWrap "  Edit Mode", txtWrap "➤ Play Mode"]


drawPickAction :: Widget Resource
drawPickAction = undefined

drawEdit :: Action -> Story -> Widget Resource
drawEdit a = undefined

drawPlay :: Story -> Widget Resource
drawPlay s = undefined
