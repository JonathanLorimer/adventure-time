module Main where

import Types
import UI (ui, AppState(..), Mode(..))

import Data.IORef
import qualified Data.Map as M
import Control.Monad.Reader
import Control.Monad.Except
import Control.Exception
import Brick.Main (defaultMain)
import Data.UUID.V4 (nextRandom)
import Data.UUID (UUID)

main :: IO ()
main = do
  dStory <- defaultStory
  initialState <- newIORef (M.fromList [("Demo Story", dStory)])
  execApp runUI initialState (\_ -> print "done")
  return ()

execApp :: Exception e
        => AppStack r e a
        -> r
        -> (a -> IO ())
        -> IO ()
execApp a r k = let e = runExceptT $ runReaderT (runApp a) r
                in e >>= either handleAppError k

handleAppError :: Exception e => e -> IO ()
handleAppError = print

runUI :: AppStack MockPersistence AppErrors AppState
runUI = do
  ref <- ask
  initialState <- liftIO $ readIORef ref
  liftIO $ defaultMain ui (buildState initialState)
  where
    buildState s = AppState PickStory s Nothing

defaultStory :: IO Story
defaultStory = do
  uuid <- nextRandom
  return Story { storyTitle = "Demo Story"
               , start      = uuid
               , passages   = M.fromList [(uuid, defaultPassage uuid)] }

defaultPassage :: UUID -> Passage
defaultPassage uuid = Passage { uuid           = uuid
                              , passageTitle   = "Passage Title"
                              , passage        = defaultPassageBody
                              , choices        = [] }

defaultPassageBody :: PassageBody
defaultPassageBody = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
