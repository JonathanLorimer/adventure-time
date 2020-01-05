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
drawUI (AppState PickStory s _ c) =
  [ C.center $ drawBorder PickStory $ drawStories c (M.elems s) ]
drawUI (AppState PickMode _ _ c) =
  [ C.center $ drawBorder PickMode [drawPickMode]]
drawUI (AppState Play _ (Just s) c) =
  [ C.center $ drawBorder Play [drawPlay s]]
drawUI (AppState (Edit Nothing) _ _ c) =
  [ C.center $ drawBorder (Edit Nothing) [drawPickAction]]
drawUI (AppState (Edit (Just a)) _ (Just s) c) =
  [ C.center $ drawBorder (Edit Nothing) [drawEdit a s]]
drawUI _ = error "something went wrong"

customAttrMap :: AttrMap
customAttrMap = attrMap V.defAttr []

handleEvent :: AppState -> BrickEvent Resource e -> EventM Resource (Next AppState)
handleEvent g (VtyEvent (V.EvKey V.KUp []))    =
  if cursor g == 0
    then continue g
    else continue $ g { cursor = cursor g - 1 }
handleEvent g (VtyEvent (V.EvKey V.KDown []))  =
  if cursor g == (M.size (stories g) - 1)
    then continue g
    else continue $ g { cursor = cursor g + 1 }
handleEvent g (VtyEvent (V.EvKey V.KEnter [])) = continue g
handleEvent g (VtyEvent (V.EvKey V.KEsc []))   = halt g
handleEvent g _                                = continue g

drawBorder :: Mode -> [Widget Resource] -> Widget Resource
drawBorder m w = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str $ "Adventure Time - Mode: " <> show m)
  $ C.center
  $ vBox w

drawStories :: CursorPos -> [Story] -> [Widget Resource]
drawStories _ [] = [txtWrap . ("➤ " <>) $ "create story"]
drawStories p ss = txtWrap <$> (fst . prefix $ showTitle . storyTitle <$> ss)
  where
    prefix l = foldr
      (\e (xs, i) -> if i == p
                      then ("➤ " <> e : xs, i - 1)
                      else ("  " <> e : xs, i - 1))
      ([], L.length l - 1) l

drawPickMode :: Widget Resource
drawPickMode = undefined

drawPickAction :: Widget Resource
drawPickAction = undefined

drawEdit :: Action -> Story -> Widget Resource
drawEdit a = undefined

drawPlay :: Story -> Widget Resource
drawPlay s = undefined
