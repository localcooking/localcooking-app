{-# LANGUAGE
    OverloadedStrings
  , ExtendedDefaultRules
  , TypeApplications
  #-}

module Auth0.Await where

import Shpadoinkle.Console (warn)
import GHCJS.Types (JSVal, JSString, jsval)
import GHCJS.Marshal (ToJSVal)
import GHCJS.Foreign.Callback (asyncCallback1)
import Control.Concurrent.STM (TMVar, newEmptyTMVarIO, putTMVar, takeTMVar, atomically)
import Control.Monad (void)
import Control.Lens ((^.))
import Language.Javascript.JSaddle.Object (js1)


default (JSString)

await :: JSVal -> IO (Either JSVal JSVal)
await promise = do
  valMVar <- newEmptyTMVarIO

  let handler :: JSVal -> IO ()
      handler val =
        atomically . putTMVar valMVar $ Right val
  handler' <- asyncCallback1 handler

  let catcher :: JSVal -> IO ()
      catcher err =
        atomically . putTMVar valMVar $ Left err
  catcher' <- asyncCallback1 catcher

  void $ promise ^. js1 "then" (jsval handler')
                 ^. js1 "catch" (jsval catcher')

  atomically $ takeTMVar valMVar

awaitThrow :: JSVal -> IO JSVal
awaitThrow promise = do
  eVal <- await promise

  case eVal of
    Right x -> pure x
    Left e -> do
      warn @ToJSVal e
      error "Promise returned an error"
