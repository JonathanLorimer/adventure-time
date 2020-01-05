module UI where

import Brick
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import Edit (Action(..))
import Data.Map (Map)
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
                         , story   :: Maybe Story }

type Resource = ()

ui :: App AppState e Resource
ui = App { appDraw         = drawUI
         , appChooseCursor = showFirstCursor
         , appHandleEvent  = undefined
         , appStartEvent   = return
         , appAttrMap      = const customAttrMap
         }

drawUI :: AppState -> [Widget Resource]
drawUI (AppState PickStory s _) =
  [ C.center $ drawBorder PickStory $ drawStories (M.elems s) ]
drawUI (AppState PickMode _ _) =
  [ C.center $ drawBorder PickMode [drawPickMode]]
drawUI (AppState Play _ (Just s)) =
  [ C.center $ drawBorder Play [drawPlay s]]
drawUI (AppState (Edit Nothing) _ _) =
  [ C.center $ drawBorder (Edit Nothing) [drawPickAction]]
drawUI (AppState (Edit (Just a)) _ (Just s)) =
  [ C.center $ drawBorder (Edit Nothing) [drawEdit a s]]
drawUI _ = error "something went wrong"

customAttrMap :: AttrMap
customAttrMap = undefined

handleEvent :: AppState -> BrickEvent Resource e -> EventM Resource (Next AppState)
handleEvent g (VtyEvent (V.EvKey V.KUp []))    = continue g
handleEvent g (VtyEvent (V.EvKey V.KDown []))  = continue g
handleEvent g (VtyEvent (V.EvKey V.KEnter [])) = continue g
handleEvent g _                                = continue g

drawBorder :: Mode -> [Widget Resource] -> Widget Resource
drawBorder m w = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str $ "Adventure Time - Mode: " <> show m)
  $ C.center
  $ vBox w

drawStories :: [Story] -> [Widget Resource]
drawStories [] = [withAttr "blue-fg" . txtWrap . ("➤ " <>) $ "create story"]
drawStories ss = (withAttr "blue-fg" . txtWrap . ("➤ " <>)) . showStory <$> ss

drawPickMode :: Widget Resource
drawPickMode = undefined

drawPickAction :: Widget Resource
drawPickAction = undefined

drawEdit :: Action -> Story -> Widget Resource
drawEdit a = undefined

drawPlay :: Story -> Widget Resource
drawPlay s = undefined
