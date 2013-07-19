{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import qualified Data.Aeson as Aeson
import Data.Conduit (($$))
import qualified Data.Conduit.List as CL
import Data.List (find)
import Data.Maybe (isJust)
import Data.Time
import qualified Facebook as FB
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Conduit
import Network.HTTP.Types

data JsonResult a = JsonOkay  a
                  | JsonError Text

instance (ToJSON a) => ToJSON (JsonResult a) where
  toJSON (JsonOkay  x) = object [ "type"    .= ("success" :: Text)
                                , "value"   .= x ]
  toJSON (JsonError y) = object [ "type"    .= ("error" :: Text)
                                , "message" .= y ]

data SignInInfo = SignInInfo { signInUserIdent :: Text
                             , signInPassword  :: Text
                             }

instance FromJSON SignInInfo where
  parseJSON (Object obj) =
    SignInInfo <$> obj .: "ident"
               <*> obj .: "password"
  parseJSON _ = fail "expected object"

data SignUpInfo = SignUpInfo { signUpUserIdent    :: Text
                             , signUpUserPassword :: Text
                             }

instance FromJSON SignUpInfo where
  parseJSON (Object obj) =
    SignUpInfo <$> obj .: "ident"
               <*> obj .: "password"
  parseJSON _ = fail "expected object"

------------------------------------------------------------------------------

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

------------------------------------------------------------------------------

postSignInR :: Handler RepJson
postSignInR = do
  signInInfo <- parseJsonBody_
  users      <- runDB $ selectKeysList [UserIdent    ==. signInUserIdent signInInfo,
                                        UserPassword ==. signInPassword  signInInfo] []
  (fmap repJson . returnJson) =<< action users
  where
    action [userId] = do
      setUserId userId
      return (JsonOkay userId)
    action _        = return (JsonError "invalid username or password")

------------------------------------------------------------------------------

postSignUpR :: Handler RepJson
postSignUpR = do
  signUpInfo <- parseJsonBody_
  eNewId     <- runDB $ insertBy $ User { userIdent    = signUpUserIdent    signUpInfo
                                        , userPassword = signUpUserPassword signUpInfo
                                        }
  (fmap repJson . returnJson) =<< action eNewId
  where
    action (Left      _) = return (JsonError "user already exists")
    action (Right newId) = do
      setUserId newId
      return (JsonOkay newId)


------------------------------------------------------------------------------

getSignOutR :: Handler ()
getSignOutR = do
  clearSession
  redirect HomeR

------------------------------------------------------------------------------

data GenFeedItem = GenFeedItem { feedItemAuthor  :: Text
                               , feedItemTime    :: UTCTime
                               , feedItemContent :: Text
                               }

instance ToJSON GenFeedItem where
  toJSON item =
    object [ "author"  .= feedItemAuthor  item
           , "time"    .= feedItemTime    item
           , "content" .= feedItemContent item
           ]

data FeedInfo = FeedInfo { feedName    :: Text
                         , feedUrl     :: Text
                         , feedAuthUrl :: Text
                         , feedIconUrl :: Text
                         }

instance ToJSON FeedInfo where
  toJSON info =
    object [ "name"    .= feedName    info
           , "url"     .= feedUrl     info
           , "authUrl" .= feedAuthUrl info
           , "iconUrl" .= feedIconUrl info
           ]

class SocialFeed feed where
  feedInfo         :: feed -> Handler FeedInfo
  isFeedAvailable  :: feed -> Handler Bool
  fetchFeedItems   :: feed -> Handler (Maybe [GenFeedItem])

serveFeed :: (SocialFeed feed) => feed -> Handler RepJson
serveFeed feed =
  fmap repJson . returnJson . maybe (JsonError "failed to fetch newsfeed") JsonOkay
    =<< fetchFeedItems feed

------------------------------------------------------------------------------


------------------------------------------------------------------------------


------------------------------------------------------------------------------

data Feeds = Feeds { knownFeeds     :: [FeedInfo]
                   , availableFeeds :: [FeedInfo]
                   }

instance ToJSON Feeds where
  toJSON feeds =
    object [ "known"     .= knownFeeds feeds
           , "available" .= availableFeeds feeds
           ]

getFeedsR :: Handler RepJson
getFeedsR = do
  repJson <$> (returnJson =<< action)
  where
    action = do
      mUserId <- maybeUserId
      case mUserId of
        Just _ -> do
          facebookFeed <- getFacebookFeed
          plusFeed     <- getPlusFeed
          feeds <- foldr ($) empty <$> sequence [checkFeed facebookFeed, checkFeed plusFeed]
          return (JsonOkay feeds)
        Nothing -> return (JsonError "not signed in")

    empty = Feeds [] []

    checkFeed :: (SocialFeed feed) => feed -> Handler (Feeds -> Feeds)
    checkFeed feed = do
      available <- isFeedAvailable feed
      info      <- feedInfo feed
      return . (if available then avail else known) $ info
      where
        avail, known :: FeedInfo -> Feeds -> Feeds
        avail info feeds = feeds { availableFeeds = info : availableFeeds feeds }
        known info feeds = feeds { knownFeeds = info : knownFeeds feeds }

------------------------------------------------------------------------------

data FacebookFeed = FacebookFeed { facebookCredentials :: FB.Credentials
                                 }

data FacebookItem = FacebookItem { facebookItemAuthor  :: FB.UserId
                                 , facebookItemTime    :: FB.FQLTime
                                 , facebookItemContent :: Text
                                 }

instance FromJSON FacebookItem where
  parseJSON (Object obj) =
    FacebookItem <$> obj .: "source_id"
                 <*> obj .: "created_time"
                 <*> obj .: "message"
  parseJSON _ = error "expected object"

instance SocialFeed FacebookFeed where
  feedInfo _ = do
    render <- getUrlRender
    return $ FeedInfo { feedName    = "Facebook"
                      , feedUrl     = render FeedsFacebookR
                      , feedAuthUrl = render FeedsFacebookAuthR
                      , feedIconUrl = render (StaticR img_icon_fb_png)
                      }

  isFeedAvailable feed = do
    mgr        <- httpManager <$> getYesod
    mFbSession <- maybeFacebookSession
    case mFbSession of
      Nothing        -> return False
      Just fbSession ->
        not <$> (FB.runFacebookT (facebookCredentials feed) mgr $ FB.hasExpired fbSession)

  fetchFeedItems feed = do
    mgr        <- httpManager <$> getYesod
    mFbSession <- maybeFacebookSession
    case mFbSession of
      Nothing        -> return Nothing
      Just fbSession -> do
        FB.runFacebookT (facebookCredentials feed) mgr $ do
          friendsPage <- FB.getUserFriends "me" [] fbSession
          friends     <- ($$ CL.consume) =<< FB.fetchAllNextPages friendsPage
          Just . map (renderFeedItem friends) . FB.pagerData <$> FB.fqlQuery query (Just fbSession)
    where
      renderFeedItem friends item =
        let friendName =
              maybe "unknown author" FB.friendName $
                find ((== facebookItemAuthor item) . FB.friendId) friends
        in GenFeedItem { feedItemAuthor  = friendName
                       , feedItemTime    = FB.unFQLTime $ facebookItemTime item
                       , feedItemContent = facebookItemContent item
                       }

      query =
        "select message,source_id,type,created_time from stream where filter_key=\"nf\" order by updated_time"


getFacebookFeed :: Handler FacebookFeed
getFacebookFeed = do
  extra <- getExtra
  return $ FacebookFeed $ FB.Credentials { FB.appName   = extraFacebookAppName   extra
                                         , FB.appId     = extraFacebookAppId     extra
                                         , FB.appSecret = extraFacebookAppSecret extra
                                         }

getFeedsFacebookR :: Handler RepJson
getFeedsFacebookR =
  serveFeed =<< getFacebookFeed

getFeedsFacebookAuthR :: Handler Html
getFeedsFacebookAuthR = do
  mgr     <- httpManager <$> getYesod
  uRender <- getUrlRender
  mCode   <- lookupGetParam "code"
  creds   <- facebookCredentials <$> getFacebookFeed
  case mCode of
    Nothing   -> do
      url <- FB.runFacebookT creds mgr $ FB.getUserAccessTokenStep1 (uRender FeedsFacebookAuthR) perms
      redirect url
    Just code -> do
      uat <- FB.runFacebookT creds mgr $ FB.getUserAccessTokenStep2 (uRender FeedsFacebookAuthR) [("code", TE.encodeUtf8 code)]
      setFacebookSession uat
      defaultLayout $ do
        toWidgetHead [julius|window.close();|]
  where
    perms :: [FB.Permission]
    perms = ["read_stream"]

facebookSessionName :: Text
facebookSessionName = "SESSION_FACEBOOK"

maybeFacebookSession :: Handler (Maybe FB.UserAccessToken)
maybeFacebookSession = do
  mData <- fmap (read . T.unpack) <$> lookupSession facebookSessionName
  case mData of
    Just (userId, uatData, time) -> do
      return (Just (FB.UserAccessToken userId uatData time))
    Nothing -> return Nothing

setFacebookSession :: FB.UserAccessToken -> Handler ()
setFacebookSession (FB.UserAccessToken userId uatData time) = do
  setSession facebookSessionName . T.pack . show $ (userId, uatData, time)

------------------------------------------------------------------------------


data PlusFeed = PlusFeed { plusClientId :: Text
                         }

data PlusFriend = PlusFriend { plusFriendId   :: Text
                             , plusFriendName :: Text
                             }

instance FromJSON PlusFriend where
  parseJSON (Object obj) =
    PlusFriend <$> obj .: "id"
               <*> obj .: "displayName"
  parseJSON _ = fail "expected object"

data PlusItem = PlusItem { plusItemUpdated :: UTCTime
                         , plusItemTitle   :: Text
                         }

instance FromJSON PlusItem where
  parseJSON (Object obj) =
    PlusItem <$> obj .: "updated"
             <*> obj .: "title"
  parseJSON _ = fail "expected object"

newtype PlusList a = PlusList { unPlusList :: [a] }

instance (FromJSON a) => FromJSON (PlusList a) where
  parseJSON (Object obj) =
    PlusList <$> obj .: "items"
  parseJSON _ = fail "expected object"

instance SocialFeed PlusFeed where
  feedInfo _ = do
    render <- getUrlRender
    return $ FeedInfo { feedName    = "Google+"
                      , feedUrl     = render FeedsPlusR
                      , feedAuthUrl = render FeedsPlusAuthR
                      , feedIconUrl = render (StaticR img_icon_plus_png)
                      }

  isFeedAvailable _ =
    isJust <$> maybePlusSession

  fetchFeedItems feed = do
    mgr   <- httpManager <$> getYesod
    mPlus <- maybePlusSession
    case mPlus of
      Nothing   -> return Nothing
      Just plus -> do
        let req = (baseReq plus) { path        = "/plus/v1/people/me/people/visible"
                                 , queryString = "maxResults=10&orderBy=best&fields=items(displayName%2Cid)&key=" <> TE.encodeUtf8 (plusClientId feed)
                                 }
        let queryFriends   = fmap concat . mapM (queryFriend plus mgr) . unPlusList
            processFriends = maybe (return Nothing) (fmap Just . queryFriends) . Aeson.decode . responseBody
        processFriends =<< httpLbs req mgr
    where
      baseReq plus =
        def { method = methodGet
            , secure = True
            , host   = "www.googleapis.com"
            , port   = 443
            , redirectCount = 3
            , requestHeaders = [("Authorization", "Bearer " <> TE.encodeUtf8 (unGoogleToken plus))]
            }

      queryFriend plus mgr friend = do
        let req = (baseReq plus) { path        = TE.encodeUtf8 ("/plus/v1/people/" <> plusFriendId friend <> "/activities/public")
                                 , queryString = "key=" <> TE.encodeUtf8 (plusClientId feed) <> "&maxResults=5&fields=items(title%2Cupdated)"
                                 }
        let renderActivities = map (renderFeedItem friend) . unPlusList
        maybe [] renderActivities . Aeson.decode . responseBody <$> httpLbs req mgr

      renderFeedItem friend activity =
        GenFeedItem { feedItemAuthor  = plusFriendName friend
                    , feedItemTime    = plusItemUpdated activity
                    , feedItemContent = plusItemTitle   activity
                    }

getPlusFeed :: Handler PlusFeed
getPlusFeed =
  PlusFeed . extraGooglePlusClientId <$> getExtra

getFeedsPlusR :: Handler RepJson
getFeedsPlusR = do
  clientId <- extraGooglePlusClientId <$> getExtra
  serveFeed $ PlusFeed { plusClientId = clientId }

newtype GoogleToken = GoogleToken { unGoogleToken :: Text }

instance FromJSON GoogleToken where
  parseJSON (Object o) =
    GoogleToken <$> o .: "access_token"
  parseJSON _ = fail "expected object"

getFeedsPlusAuthR :: Handler Html
getFeedsPlusAuthR = do
  mCode        <- lookupGetParam "code"
  clientId     <- extraGooglePlusClientId <$> getExtra
  clientSecret <- extraGooglePlusSecret <$> getExtra
  case mCode of
    Nothing -> do
      let url = T.concat [ "https://accounts.google.com/o/oauth2/auth?"
                         , "scope=https%3A%2F%2Fwww.googleapis.com%2Fauth%2Fplus.login+https%3A%2F%2Fwww.googleapis.com%2Fauth%2Fplus.me&"
                         , "redirect_uri=http%3A%2F%2Flocalhost%3A3000%2Ffeeds%2Fplus%2Fauth&"
                         , "response_type=code&"
                         , "client_id=" <> clientId <> "&"
                         , "approval_prompt=force"
                         ]
      redirect url
    Just code -> do
      let req = def { method         = methodPost
                    , secure         = True
                    , host           = "accounts.google.com"
                    , path           = "/o/oauth2/token"
                    , port           = 443
                    , redirectCount  = 3
                    , requestHeaders = [("Content-Type", "application/x-www-form-urlencoded")]
                    , requestBody    = RequestBodyBS $ TE.encodeUtf8 $
                                         T.concat [ "code=" <> code <> "&"
                                                  , "client_id=" <> clientId <> "&"
                                                  , "client_secret=" <> clientSecret <> "&"
                                                  , "redirect_uri=http://localhost:3000/feeds/plus/auth&"
                                                  , "grant_type=authorization_code"
                                                  ]
                    }
      mgr <- httpManager <$> getYesod
      res <- httpLbs req mgr
      case Aeson.decode (responseBody res) of
        Just token -> do
          setPlusSession token
          defaultLayout $ do
            toWidgetHead [julius|window.close();|]
        Nothing -> defaultLayout $ do
          toWidget [whamlet|Failed to parse response from Google|]

plusSessionName :: Text
plusSessionName = "SESSION_PLUS"

maybePlusSession :: Handler (Maybe GoogleToken)
maybePlusSession = do
  fmap GoogleToken <$> lookupSession plusSessionName

setPlusSession :: GoogleToken -> Handler ()
setPlusSession (GoogleToken token) =
  setSession plusSessionName token
