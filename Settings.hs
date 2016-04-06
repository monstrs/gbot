{-# Language CPP #-}

module Settings where

import ClassyPrelude.Yesod
import Control.Exception           (throw)
import Data.Aeson                  (Result (..), fromJSON, withObject, (.!=),
                                    (.:?))
import Data.FileEmbed              (embedFile)
import Data.Yaml                   (decodeEither')
import Database.Persist.MongoDB    (MongoConf)
import Network.Wai.Handler.Warp    (HostPreference)
import Yesod.Default.Config2       (applyEnvValue, configSettingsYml)

data OAuthKeys = OAuthKeys
    { oauthKeysClientId     :: Text
    , oauthKeysClientSecret :: Text
    }

data AppSettings = AppSettings
    { appDatabaseConf           :: MongoConf
    , appRoot                   :: Maybe Text
    , appHost                   :: HostPreference
    , appPort                   :: Int
    , appIpFromHeader           :: Bool
    , appDetailedRequestLogging :: Bool
    , appShouldLogAll           :: Bool
    }

instance FromJSON AppSettings where
    parseJSON = withObject "AppSettings" $ \o -> do
        let defaultDev =
#if DEVELOPMENT
                True
#else
                False
#endif
        appDatabaseConf           <-                o .:  "database"
        appRoot                   <-                o .:? "approot"
        appHost                   <- fromString <$> o .:  "host"
        appPort                   <-                o .:  "port"
        appIpFromHeader           <-                o .:  "ip-from-header"
        appDetailedRequestLogging <-                o .:? "detailed-logging" .!= defaultDev
        appShouldLogAll           <-                o .:? "should-log-all"   .!= defaultDev

        return AppSettings {..}

configSettingsYmlBS :: ByteString
configSettingsYmlBS = $(embedFile configSettingsYml)

configSettingsYmlValue :: Value
configSettingsYmlValue = either throw id $ decodeEither' configSettingsYmlBS

compileTimeAppSettings :: AppSettings
compileTimeAppSettings =
    case fromJSON $ applyEnvValue False mempty configSettingsYmlValue of
        Error e -> error e
        Success settings -> settings
