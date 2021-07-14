{-# LANGUAGE
    RecordWildCards
  , OverloadedStrings
  , ExtendedDefaultRules
  , TypeApplications
  #-}

module Auth0 where

import Auth0.Await (await, awaitThrow)

import Shpadoinkle.Console (warn)
import GHCJS.Types (JSString, JSVal)
import GHCJS.Marshal (ToJSVal (toJSVal), fromJSValUnchecked)
import Control.Lens ((^.))
import Control.Monad (void)
import Control.Exception.Base (catch)
import JavaScript.Object (create, unsafeSetProp)
import Language.Javascript.JSaddle.Object (js0, js1)


default (JSString)

foreign import javascript unsafe "createAuth0Client" createAuth0Client' :: JSVal -> IO JSVal

data CreateAuth0Client = CreateAuth0Client
  { domain :: JSString
  , clientId :: JSString
  , redirectUri :: JSString
  }
instance ToJSVal CreateAuth0Client where
  toJSVal CreateAuth0Client{..} = do
    o <- create
    domain' <- toJSVal domain
    clientId' <- toJSVal clientId
    redirectUri' <- toJSVal redirectUri
    unsafeSetProp "domain" domain' o
    unsafeSetProp "client_id" clientId' o
    unsafeSetProp "redirect_uri" redirectUri' o
    toJSVal o

newtype Auth0 = Auth0 {getAuth0 :: JSVal}

createAuth0Client :: CreateAuth0Client -> IO Auth0
createAuth0Client args =
  fmap Auth0 . awaitThrow =<< createAuth0Client' =<< toJSVal args


login :: Auth0 -> IO ()
login (Auth0 auth0) =
  void $ awaitThrow =<< auth0 ^. js0 "loginWithRedirect"

newtype RedirectResult = RedirectResult {getRedirectResult :: JSVal}


handleRedirectCallback :: Auth0 -> IO (Maybe RedirectResult)
handleRedirectCallback (Auth0 auth0) = do
  eRes <- await =<< auth0 ^. js0 "handleRedirectCallback"
  case eRes of
    Left e -> do
      warn @ToJSVal e
      pure Nothing
    Right x -> pure . Just $ RedirectResult x


-- FIXME marshal into a haskell value
-- NOTE can be undefined, or an object of various vendor supported fields
getUser :: Auth0 -> IO (Maybe JSVal)
getUser (Auth0 auth0) = do
  eRes <- await =<< auth0 ^. js0 "getUser"
  case eRes of
    Left e -> do
      warn @ToJSVal e
      pure Nothing
    Right x -> pure $ Just x


getTokenSilently :: Auth0 -> IO (Maybe JSString)
getTokenSilently (Auth0 auth0) = do
  eRes <- await =<< auth0 ^. js0 "getTokenSilently"
  case eRes of
    Left e -> do
      warn @ToJSVal e
      pure Nothing
    Right x -> Just <$> fromJSValUnchecked x


logout :: Auth0
       -> Maybe JSString -- ^ Allowed logout URL
       -> IO ()
logout (Auth0 auth0) mLogoutURL =
  void $ case mLogoutURL of
    Nothing -> auth0 ^. js0 "logout"
    Just logoutUrl -> do
      o <- create
      logoutUrl' <- toJSVal logoutUrl
      unsafeSetProp "returnTo" logoutUrl' o
      auth0 ^. js1 "logout" o
