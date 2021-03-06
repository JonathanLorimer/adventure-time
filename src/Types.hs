{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Types
( AppStack(..)
, AppErrors(..)
, MockPersistence
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
, AppState(..) , Resource(..)
, CursorPos
, Mode(..)
, Action(..)
, PassageForm(..)
, formChoices
, formPassage
, formPassageTitle
, formSubmit
, Persistence(..)
, StoryPersistence
) where

import           Brick.Forms
import           Control.Exception
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.IORef
import qualified Data.List            as L
import           Data.Map             (Map)
import           Data.Maybe
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Text.Lazy       (toStrict)
import           Data.UUID            (UUID)
import           Lens.Micro.TH        (makeLenses)
import           Text.Pretty.Simple
-- Monad Stack

type MockPersistence = IORef (Map Title Story)

data Persistence a = Persistence { get :: IO a
                                 , put :: (a -> a) -> IO ()
                                 }

type StoryPersistence = Persistence (Map Title Story)

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

-- Business Logic
newtype ID a = ID UUID deriving (Eq, Ord, Show)

data Passage = Passage { uuid         :: ID Passage
                       , passageTitle :: Title
                       , passage      :: PassageBody
                       , choices      :: [ID Passage] }
                       deriving (Eq, Ord, Show)

data Story = Story { storyTitle :: Title
                   , start      :: ID Passage
                   , passages   :: Map (ID Passage) Passage }
                   deriving (Eq, Ord, Show)

type Title           = Text
type PassageBody     = Text




-- UI State
data Mode = PickStory
          | PickMode
          | Play
          | Edit (Maybe Action)
          deriving (Eq, Ord, Show)

data AppState e = AppState { mode        :: Mode
                           , stories     :: Map Title Story
                           , story       :: Maybe Story
                           , curPassage  :: Maybe Passage
                           , cursor      :: CursorPos
                           , passageForm :: Maybe (Form PassageForm e Resource)
                           , persistence :: StoryPersistence }

instance Show (AppState e) where
  showsPrec _ AppState { mode, stories, story, curPassage, cursor } = showString
    $ "mode: " <> show mode <> "\n"
    <> "stories: " <> (T.unpack . toStrict $ pShow stories) <> "\n"
    <> "story: " <> (show $ storyTitle <$> story) <> "\n"
    <> "curPassage: " <> (show $ passageTitle <$> curPassage) <> "\n"
    <> "cursor: " <> show cursor


data Resource = TitleField
              | PassageField
              | ChoicesField String
              | SubmitField
                deriving (Eq, Ord, Show)

type CursorPos = Int


-- Edit State
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

-- Form State
data PassageForm = PassageForm { _formPassageTitle :: Text
                               , _formPassage      :: Text
                               , _formChoices      :: Map (ID Passage) Bool
                               , _formSubmit       :: Bool
                               } deriving (Eq, Ord, Show)

makeLenses ''PassageForm

-- Type Coercion
showStory :: Story -> Text
showStory = T.pack . show

showTitle :: Title -> Text
showTitle = T.pack . show

showPassageBody :: PassageBody -> Text
showPassageBody = T.pack . show

showAction :: Action -> Text
showAction = T.pack . show
