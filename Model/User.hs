{-# OPTIONS_GHC -fno-warn-orphans #-}

module Model.User
    ( User(..)
    , authenticateUser
    , getUserById
    , getUserByToken
    ) where

import Import.NoFoundation

import Data.Text.Instances      ()
import Database.MongoDB         (MongoContext)
import Database.Persist.MongoDB (oidToKey, readMayObjectId)

data Profile = Profile
    { profileName           :: Text
    , profileEmail          :: Text
    , profileAccessToken    :: Text
    }

instance ToJSON (Entity User) where
    toJSON (Entity uid u) = object
        [ "id" .= String (toPathPiece uid)
        , "ident" .= userIdent u
        , "name" .= userName u
        , "email" .= userEmail u
        ]

getUserById :: MonadIO m => Text -> ReaderT MongoContext m (Maybe User)
getUserById uid =
    case idToKey uid of
        (Just oid) -> do
            res <- selectList [UserId ==. oid] [LimitTo 1]
            case res of
                [Entity _ u] -> return $ Just u
                _ -> return Nothing
        Nothing -> return Nothing

getUserByToken :: MonadIO m => Maybe Text -> ReaderT MongoContext m (Maybe (Entity User))
getUserByToken token = getBy $ UserByAccessToken $ fromMaybe "" token

authenticateUser :: (MonadIO m, AuthId master ~ Key User) =>
                    Creds m1 -> ReaderT MongoContext m (AuthenticationResult master)
authenticateUser creds@Creds{..} = do
    muser <- getBy $ UserByIdent credsIdent

    let euser = credsToUser creds
        muserId = entityKey <$> muser

    maybe (authNew euser) (authExisting euser) muserId

  where
    authNew (Left err) = return $ ServerError $ credsPlugin ++ ": " ++ err
    authNew (Right user) = Authenticated <$> insert user

    authExisting euser userId = do
        mapM_ (replace userId) euser
        return $ Authenticated userId

credsToUser :: Creds m -> Either Text User
credsToUser Creds{..} = User
    <$> pure credsIdent
    <*> (profileName <$> eprofile)
    <*> (profileEmail <$> eprofile)
    <*> (profileAccessToken <$> eprofile)
  where
    eprofile = githubProfile credsExtra

githubProfile :: [(Text, Text)] -> Either Text Profile
githubProfile extra = Profile
    <$> (lookupExtra "name" extra <|> lookupExtra "login" extra)
    <*> lookupExtra "email" extra
    <*> lookupExtra "access_token" extra

lookupExtra :: Text -> [(Text, b)] -> Either Text b
lookupExtra k extra =
    maybe (Left $ "missing key " ++ k) Right $ lookup k extra

idToKey :: Text -> Maybe (Key User)
idToKey uid = oidToKey <$> readMayObjectId uid
