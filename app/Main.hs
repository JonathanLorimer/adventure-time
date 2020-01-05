module Main where

import Types

import Control.Monad.Reader
import Control.Monad.Except
import Control.Exception

main :: IO ()
main = print "give me functionality"

execApp :: Exception e
        => App r e a
        -> r
        -> (a -> IO ())
        -> IO ()
execApp a r k = let e = runExceptT $ runReaderT (runApp a) r
                in e >>= either handleAppError k

handleAppError :: Exception e => e -> IO ()
handleAppError = print
