{-# LANGUAGE ForeignFunctionInterface #-}
module Main
    ( runAlienator
    )
  where

import Control.Monad.Trans
import Reflex.Cocos2d

runAlienator :: IO ()
runAlienator = mainScene $ do
      liftIO $ putStrLn "Running."

foreign export ccall runAlienator :: IO ()
