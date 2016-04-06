module Handler.Users where

import Import
import Model.User

getUsersMeR :: Handler Value
getUsersMeR = do
    me <- maybeAuth
    returnJson me

getUsersTokenR :: Handler Value
getUsersTokenR = do
    key <- lookupSession credsKey
    deleteSession credsKey

    case key of
        (Just uid) -> do
            entity <- runDB $ getUserById uid
            case entity of
                (Just u) -> return $ object ["access_token" .= userAccessToken u]
                Nothing -> sendResponseStatus status401 $ object []
        Nothing -> sendResponseStatus status401 $ object []
