module Model where

import Prelude
import Yesod
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import Database.Persist.Quasi
import Data.Typeable (Typeable)

class (Aeson.ToJSON (FeedItem site), Aeson.FromJSON (FeedItem site)) => SocialFeed site where
  data FeedSource site
  data FeedItem   site

  fetchFeedSources :: site -> [FeedSource site]
  fetchFeedItems   :: FeedSource site -> [FeedItems site]


-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlOnlySettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")
