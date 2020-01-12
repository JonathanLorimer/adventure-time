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
, ID(..)
) where

import Data.Map (Map)
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

showStory :: Story -> Text
showStory = T.pack . show

showTitle :: Title -> Text
showTitle = T.pack . show

showPassageBody :: PassageBody -> Text
showPassageBody = T.pack . show

