{-# LANGUAGE TypeApplications #-}
module Main where

import           Types
import           UI                   (ui)

import           Brick.Main           (defaultMain)
import           Control.Exception
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.IORef
import qualified Data.Map             as M
import qualified Data.Text            as T
import           Data.UUID            (UUID)
import           Data.UUID.V4         (nextRandom)

main :: IO ()
main = do
  dStory  <- defaultStory
  dStory2 <- defaultStory2
  ref <- newIORef $ M.fromList [ ("Demo Story" :: Title , dStory)
                               , ("Demo Story 2" :: Title, dStory2) ]
  let persist = Persistence { get = readIORef ref
                            , put = modifyIORef' ref
                            }
  execApp runUI persist print
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

runUI :: AppStack (Persistence (M.Map Title Story))  AppErrors (AppState e)
runUI = do
  persistence <- ask
  initialState <- liftIO $ get persistence
  liftIO $ defaultMain ui (buildState initialState persistence)
  where
    buildState s = AppState PickStory s Nothing Nothing 0 Nothing

defaultStory :: IO Story
defaultStory = do
  passageId <- nextRandom
  return Story { storyTitle = "Demo Story"
               , start      = ID passageId
               , passages   = M.fromList [(ID passageId, defaultPassage "Passage 1" passageId)] }

defaultStory2 :: IO Story
defaultStory2 = do
  passageId <- nextRandom
  passageId2 <- nextRandom
  passageId3 <- nextRandom
  passageId4 <- nextRandom

  let p1 = (defaultPassage "Passage 1" passageId ) { choices = [ ID passageId2
                                                   , ID passageId3
                                                   , ID passageId4 ] }
  let p2 = (defaultPassage "Passage 2" passageId2) { choices = [ ID passageId3 ] }
  let p3 = (defaultPassage "Passage 3" passageId3) { choices = [ ID passageId
                                                   , ID passageId4 ] }
  let p4 = (defaultPassage "Passage 4" passageId4) { choices = [] }

  return Story { storyTitle = "Demo Story 2"
               , start      = ID passageId
               , passages   = M.fromList [(ID passageId , p1)
                                         ,(ID passageId2, p2)
                                         ,(ID passageId3, p3)
                                         ,(ID passageId4, p4)]}

defaultPassage :: T.Text -> UUID -> Passage
defaultPassage title passageId = Passage { uuid           = ID passageId
                                         , passageTitle   = title
                                         , passage        = defaultPassageBody
                                         , choices        = [] }

defaultPassageBody :: PassageBody
defaultPassageBody = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
