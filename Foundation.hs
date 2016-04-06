module Foundation where

import Database.Persist.MongoDB hiding (master)

import Control.Monad.Trans.Maybe
import Import.NoFoundation
import Yesod.Core.Types             (Logger)
import Yesod.Auth.OAuth2.Github

import qualified Yesod.Core.Unsafe as Unsafe

import Model.User

data App = App
    { appSettings           :: AppSettings
    , appConnPool           :: ConnectionPool
    , appHttpManager        :: Manager
    , appLogger             :: Logger
    , appGithubOAuthKeys    :: OAuthKeys
    }

mkYesodData "App" $(parseRoutesFile "config/routes")

instance Yesod App where
    approot = ApprootRequest $ \app req ->
        (fromMaybe (getApprootText guessApproot app req)
            (appRoot $ appSettings app))

    yesodMiddleware = defaultYesodMiddleware

    authRoute _ = Just UsersTokenR

    isAuthorized (AuthR _) _ = return Authorized
    isAuthorized UsersTokenR _ = return Authorized

    isAuthorized _ _ = do
        mauth <- maybeAuth
        case mauth of
            Nothing -> return AuthenticationRequired
            Just _  -> return Authorized

    shouldLog app _source level =
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger = return . appLogger

instance YesodPersist App where
    type YesodPersistBackend App = MongoContext
    runDB action = do
        master <- getYesod
        let dbconf = mgAccessMode $ appDatabaseConf $ appSettings master
            p = appConnPool master
        runMongoDBPool dbconf action p

instance YesodAuth App where
    type AuthId App = UserId
    loginDest _ = UsersTokenR
    logoutDest _ = UsersTokenR

    maybeAuthId = runMaybeT $ do
        token <- lookupGetParam "token"
        usr <- MaybeT $ runDB $ getUserByToken token
        return $ entityKey usr

    authenticate = runDB . authenticateUser

    authHttpManager = appHttpManager

    authPlugins m =
        [ oauth2Github
            (oauthKeysClientId $ appGithubOAuthKeys m)
            (oauthKeysClientSecret $ appGithubOAuthKeys m)
        ]

instance YesodAuthPersist App where
    type AuthEntity App = User

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

instance HasHttpManager App where
    getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger
