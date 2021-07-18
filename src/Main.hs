{-# LANGUAGE
    OverloadedStrings
  , RecordWildCards
  , TypeApplications
  , ExtendedDefaultRules
  , NamedFieldPuns
  , ScopedTypeVariables
  , RankNTypes
  , DeriveGeneric
  , GeneralizedNewtypeDeriving
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
import           Control.DeepSeq                            (NFData (rnf))
import           GHC.Generics                               (Generic)
import qualified GHCJS.Types                                as T
import           GHCJS.Foreign.Callback                     (asyncCallback1)
import           GHCJS.Marshal                              (ToJSVal (toJSVal), FromJSVal (fromJSVal))
import qualified Language.Javascript.JSaddle.Object         as JS
import           System.IO.Unsafe                           (unsafePerformIO)

default (T.JSString)

foreign import javascript unsafe "$1 === $2" equalsJSVal :: T.JSVal -> T.JSVal -> Bool

newtype UserInfo = UserInfo {getUserInfo :: JS.Object}
  deriving (Generic, ToJSVal)
instance NFData UserInfo where
  rnf (UserInfo x) = seq x ()
instance FromJSVal UserInfo where
  fromJSVal x = Just . UserInfo <$> JS.makeObject x
instance Eq UserInfo where
  (UserInfo x) == (UserInfo y) = unsafePerformIO $
    equalsJSVal <$> toJSVal x <*> toJSVal y


data State = State
  { loginInfo :: Maybe UserInfo
  } deriving (Eq, Generic)
instance NFData State

view :: forall m. MonadIO m => Auth0 -> State -> Html m State
view auth0 State{loginInfo} =
  div_ $
    [ "hello world"
    ] <> loginHeader
  where
    loginHeader :: [Html m State]
    loginHeader = case loginInfo of
      Nothing ->
        [ button [onClickM_ (liftIO $ login auth0)] ["Login"]
        ]
      Just userInfo ->
        let checkLogin = do
              putStrLn "User Info"
              warn @ToJSVal userInfo
        in  [ button [onClickM_ (liftIO $ logout auth0 Nothing)] ["Logout"]
            , button [onClickM_ (liftIO $ checkLogin)] ["Check Login"]
            ]


main :: IO ()
main = do
  putStrLn "Calling createAuth0Client"

  auth0 <- createAuth0Client $ CreateAuth0Client
    { domain = "localcooking-dev.us.auth0.com"
    , clientId = "Q0ITViiiUSflk71dTs8Zr9I3mfhwu35h"
    , redirectUri = "https://localcooking.com"
    , cacheLocation = Nothing
    , useRefreshTokens = Nothing
    }

  void $ handleRedirectCallback auth0
  mUser <- getUser auth0

  runJSorWarp 8080 $
    let initialState :: State
        initialState = State
          { loginInfo = mUser
          }
    in  simple runSnabbdom initialState (view auth0) stage
