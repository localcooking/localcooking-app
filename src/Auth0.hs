{-# LANGUAGE
    RecordWildCards
  , OverloadedStrings
  , ExtendedDefaultRules
  , TypeApplications
  #-}

module Auth0 where

import Auth0.Await (await, awaitThrow)

import Shpadoinkle.Console (warn)
import GHCJS.Types (JSString, JSVal, isUndefined, isNull)
import GHCJS.Marshal (ToJSVal (toJSVal), FromJSVal (fromJSVal, fromJSValUnchecked))
import Control.Lens ((^.))
import Control.Monad (void)
import Control.Exception.Base (catch)
import JavaScript.Object (create, unsafeSetProp)
import Language.Javascript.JSaddle.Object (Object, js0, js1)
import Language.Javascript.JSaddle.Classes.Internal (makeObject)
import Language.Javascript.JSaddle.Types (ghcjsPure)


default (JSString)

foreign import javascript unsafe "createAuth0Client" createAuth0Client' :: JSVal -> IO JSVal

-- | Sets the cache location for Auth0. See <https://github.com/auth0/auth0-spa-js#data-caching-options the docs> for details. The default value is 'Memory'. See also <https://github.com/auth0/auth0-spa-js#creating-a-custom-cache the docs on custom caches>.
data CacheLocation = Memory | LocalStorage
instance ToJSVal CacheLocation where
  toJSVal x = toJSVal $ case x of
    Memory -> "memory" :: JSString
    LocalStorage -> "localstorage"

data CreateAuth0Client = CreateAuth0Client
  { domain :: JSString
  , clientId :: JSString
  , redirectUri :: JSString
  , cacheLocation :: Maybe CacheLocation
  , useRefreshTokens :: Maybe Bool -- ^ See <https://github.com/auth0/auth0-spa-js#refresh-tokens the docs> for details.
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
    case cacheLocation of
      Nothing -> pure ()
      Just cacheLocation' -> do
        cacheLocation'' <- toJSVal cacheLocation'
        unsafeSetProp "cacheLocation" cacheLocation'' o
    case useRefreshTokens of
      Nothing -> pure ()
      Just useRefreshTokens' -> do
        useRefreshTokens'' <- toJSVal useRefreshTokens'
        unsafeSetProp "useRefreshTokens" useRefreshTokens'' o
    toJSVal o

newtype Auth0 = Auth0 {getAuth0 :: JSVal}

createAuth0Client :: CreateAuth0Client -> IO Auth0
createAuth0Client args =
  fmap Auth0 . awaitThrow =<< createAuth0Client' =<< toJSVal args


-- FIXME apply parameter - https://auth0.com/docs/libraries/auth0-single-page-app-sdk
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
getUser :: FromJSVal a => Auth0 -> IO (Maybe a)
getUser (Auth0 auth0) = do
  eRes <- await =<< auth0 ^. js0 "getUser"
  case eRes of
    Left e -> do
      warn @ToJSVal e
      pure Nothing
    Right x -> do
      undef <- ghcjsPure $ isUndefined x
      null <- ghcjsPure $ isNull x
      if undef || null
        then pure Nothing
        else fromJSVal x


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
