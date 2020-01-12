{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types
( AppStack(..) , AppErrors(..) , MockPersistence
, Title
, PassageBody
, Story(..)
, Passage(..)
, showStory
, showTitle
, showPassageBody
, showAction
, actionTable
, ID(..)
, AppState(..)
, Resource
, CursorPos
, Mode(..)
, Action(..)
) where

import qualified Data.List as L
import Data.Map (Map)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)
import Data.IORef
import Control.Monad.Reader
import Control.Monad.Except
import Control.Exception

newtype ID a = ID UUID deriving (Eq, Ord, Show)

data Passage = Passage { uuid           :: ID Passage
                       , passageTitle   :: Title
                       , passage        :: PassageBody
                       , choices        :: [ID Passage] }
                       deriving (Eq, Ord, Show)

data Story = Story { storyTitle :: Title
                   , start      :: ID Passage
                   , passages   :: Map (ID Passage) Passage }
                   deriving (Eq, Ord, Show)

type Title           = Text
type PassageBody     = Text
type MockPersistence = IORef (Map Title Story)

data AppErrors       = BasicError
  deriving (Show)

instance Exception AppErrors

newtype AppStack r e a = AppStack { runApp :: ReaderT r (ExceptT e IO) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadError e
           , MonadReader r
           , MonadIO )

data Mode = PickStory
          | PickMode
          | Play
          | Edit (Maybe Action)
          deriving (Eq, Ord, Show)

data AppState = AppState { mode       :: Mode
                         , stories    :: Map Title Story
                         , story      :: Maybe Story
                         , curPassage :: Maybe Passage
                         , cursor     :: CursorPos }

type Resource = ()
type CursorPos = Int


data Action = AddPassage
            | RemovePassage
            | EditPassage (Maybe Passage)
            deriving (Eq, Ord)

instance Enum Action where
  fromEnum = fromJust . (`L.elemIndex` actionTable)
  toEnum   = (actionTable L.!!)

instance Show Action where
  showsPrec _  AddPassage      = showString "Add Passage"
  showsPrec _  RemovePassage   = showString "Remove Passage"
  showsPrec _  (EditPassage _) = showString "Edit Passage"

actionTable :: [Action]
actionTable = [AddPassage, RemovePassage, EditPassage Nothing]

showStory :: Story -> Text
showStory = T.pack . show

showTitle :: Title -> Text
showTitle = T.pack . show

showPassageBody :: PassageBody -> Text
showPassageBody = T.pack . show

showAction :: Action -> Text
showAction = T.pack . show
