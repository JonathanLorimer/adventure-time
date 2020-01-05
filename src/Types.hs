{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types
( AppStack(..)
, AppErrors(..)
, MockPersistence
, Title
, PassageBody
, Story(..)
, Passage(..)
) where

import Data.Map (Map)
import Data.Text (Text)
import Data.UUID (UUID)
import Data.IORef
import Control.Monad.Reader
import Control.Monad.Except

data Passage = Passage { uuid           :: UUID
                       , passageTitle   :: Title
                       , passage        :: PassageBody
                       , choices        :: [UUID] }
                       deriving (Eq, Ord, Show)

data Story = Story { storyTitle :: Title
                   , start      :: UUID
                   , passages   :: Map UUID Passage }
                   deriving (Eq, Ord, Show)

type Title           = Text
type PassageBody     = Text
type MockPersistence = IORef (Map Title Story)

data AppErrors       = BasicError

newtype AppStack r e a = AppStack { runApp :: ReaderT r (ExceptT e IO) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadError e
           , MonadReader r
           , MonadIO )

