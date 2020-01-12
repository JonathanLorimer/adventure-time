module Main where

import Types
import UI (ui)

import Data.IORef
import qualified Data.Text as T
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import qualified Data.Map as M
import Control.Monad.Reader
import Control.Monad.Except
import Control.Exception
import Brick.Main (defaultMain)

main :: IO ()
main = do
  dStory  <- defaultStory
  dStory2 <- defaultStory2
  initialState <- newIORef $ M.fromList [ ("Demo Story", dStory)
                                        , ("Demo Story 2", dStory2) ]
  execApp runUI initialState (\_ -> print ("done" :: String))
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
    buildState s = AppState PickStory s Nothing Nothing 0

defaultStory :: IO Story
defaultStory = do
  passageId <- nextRandom
  return Story { storyTitle = "Demo Story"
               , start      = ID passageId
               , passages   = M.fromList [(ID passageId, defaultPassage passageId)] }

defaultStory2 :: IO Story
defaultStory2 = do
  passageId <- nextRandom
  passageId2 <- nextRandom
  passageId3 <- nextRandom
  passageId4 <- nextRandom

  let p1 = (defaultPassage passageId ) { choices = [ ID passageId2
                                                   , ID passageId3
                                                   , ID passageId4 ] }
  let p2 = (defaultPassage passageId2) { choices = [ ID passageId3 ] }
  let p3 = (defaultPassage passageId3) { choices = [ ID passageId
                                                   , ID passageId4 ] }
  let p4 = (defaultPassage passageId4) { choices = [] }

  return Story { storyTitle = "Demo Story 2"
               , start      = ID passageId
               , passages   = M.fromList [(ID passageId , p1)
                                         ,(ID passageId2, p2)
                                         ,(ID passageId3, p3)
                                         ,(ID passageId4, p4)]}

defaultPassage :: UUID -> Passage
defaultPassage passageId = Passage { uuid           = ID passageId
                                   , passageTitle   = T.pack $ "Passage: " <> show passageId
                                   , passage        = defaultPassageBody
                                   , choices        = [] }

defaultPassageBody :: PassageBody
defaultPassageBody = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
