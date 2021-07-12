{-# LANGUAGE
    OverloadedStrings
  , RecordWildCards
  , TypeApplications
  , ExtendedDefaultRules
  #-}


module Main where

import Auth0 (createAuth0Client, login, logout, handleRedirectCallback, getUser, Auth0, CreateAuth0Client (..))

import           Shpadoinkle                                (Html, JSM)
import           Shpadoinkle.Backend.Snabbdom               (runSnabbdom, stage)
import           Shpadoinkle.Html
import           Shpadoinkle.Run                            (runJSorWarp, simple)
import           Shpadoinkle.Console                        (warn)
import           Control.Lens                               ((^.))
import           Control.Monad                              (void)
import           Control.Monad.IO.Class                     (MonadIO (liftIO))
import qualified GHCJS.Types                                as T
import           GHCJS.Foreign.Callback                     (asyncCallback1)
import           GHCJS.Marshal                              (ToJSVal (toJSVal))
import qualified Language.Javascript.JSaddle.Object         as JS

default (T.JSString)




view :: MonadIO m => Auth0 -> () -> Html m ()
view auth0 _ =
  div_
    [ "hello world"
    , button [onClickM_ (liftIO $ login auth0)] ["Login"]
    , button [onClickM_ (liftIO $ logout auth0 Nothing)] ["Logout"]
    , button [onClickM_ (liftIO $ checkLogin)] ["Check Login"]
    ]
  where
    checkLogin = do
      void $ handleRedirectCallback auth0
      -- warn @ToJSVal auth0
      user <- getUser auth0

      warn @ToJSVal user


app :: Auth0 -> JSM ()
app auth0 = simple runSnabbdom () (view auth0) stage


main :: IO ()
main = do
  putStrLn "Calling createAuth0Client"

  auth0 <- createAuth0Client $ CreateAuth0Client
    { domain = "localcooking-dev.us.auth0.com"
    , clientId = "Q0ITViiiUSflk71dTs8Zr9I3mfhwu35h"
    , redirectUri = "https://localcooking.com"
    }

  runJSorWarp 8080 (app auth0)
