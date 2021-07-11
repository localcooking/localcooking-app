{-# LANGUAGE
    OverloadedStrings
  , RecordWildCards
  , TypeApplications
  , ExtendedDefaultRules
  #-}


module Main where


import           Shpadoinkle                                (Html, JSM)
import           Shpadoinkle.Backend.Snabbdom               (runSnabbdom, stage)
import           Shpadoinkle.Html
import           Shpadoinkle.Run                            (live, runJSorWarp, simple)
import           Shpadoinkle.Console                        (warn)
import           Control.Lens                               ((^.))
import qualified GHCJS.Types                                as T
import           GHCJS.Marshal                              (ToJSVal (toJSVal))
import qualified Language.Javascript.JSaddle.Object         as JS

default (T.JSString)

foreign import javascript unsafe "createAuth0Client" createAuth0Client' :: T.JSVal -> IO T.JSVal

data CreateAuth0Client = CreateAuth0Client
  { domain :: T.JSString
  , clientId :: T.JSString
  , redirectUrl :: T.JSString
  }
instance ToJSVal CreateAuth0Client where
  toJSVal CreateAuth0Client{..} = do
    o <- JS.create
    domain' <- toJSVal domain
    clientId' <- toJSVal clientId
    redirectUrl' <- toJSVal redirectUrl
    JS.unsafeSetProp "domain" domain' o
    JS.unsafeSetProp "client_id" clientId' o
    JS.unsafeSetProp "redirect_url" redirectUrl' o
    toJSVal o



createAuth0Client :: CreateAuth0Client -> IO ()
createAuth0Client args = do
  o' <- createAuth0Client' =<< toJSVal args

  let handler :: JS.JSCallAsFunction
      handler _ _ [auth0] =
        putStrLn "yoooo"
      handler _ _ _ = error "expecting one arg"
  handler' <- toJSVal handler

  let catcher :: JS.JSCallAsFunction
      catcher _ _ errs =
        warn @ToJSVal errs
  catcher' <- toJSVal catcher

  o'' <- o' ^. JS.js1 "then" handler'
  o''' <- o'' ^. JS.js1 "catch" catcher'
  pure ()


view :: () -> Html m ()
view _ =
  div_
    [ "hello world"
    -- , button []
    ]


app :: JSM ()
app = simple runSnabbdom () view stage


dev :: IO ()
dev = live 8080 app


main :: IO ()
main = do
  putStrLn "\nhi, my name is localcooking-app"
  putStrLn "happy point of view on https://localhost:8080\n"
  runJSorWarp 8080 app
