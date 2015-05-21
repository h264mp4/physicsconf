{-#LANGUAGE ViewPatterns #-}
module Foundation where

import Prelude
import Yesod
import Yesod.Static
import Yesod.Auth
import Yesod.Form.Jquery
import Yesod.Default.Config
import Yesod.Default.Util (addStaticContentExternal)
import Network.HTTP.Client.Conduit (Manager, HasHttpManager (getHttpManager))
import qualified Settings
import Settings.Development (development)
import qualified Database.Persist
import Database.Persist.Sql (SqlBackend, insert)
import Settings.StaticFiles
import Settings (widgetFile, Extra (..))
import Model
import Text.Jasmine (minifym)
import Text.Hamlet (hamletFile)
import Yesod.Core.Types (Logger)
import Data.Time
import Data.Text(Text)
import System.Locale


import Handler.MiscTypes

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { settings :: AppConfig DefaultEnv Extra
    , getStatic :: Static -- ^ Settings for static file serving.
    , connPool :: Database.Persist.PersistConfigPool Settings.PersistConf -- ^ Database connection pool.
    , httpManager :: Manager
    , persistConfig :: Settings.PersistConf
    , appLogger :: Logger
    }

instance HasHttpManager App where
    getHttpManager = httpManager

-- add our owns
prettyTime :: UTCTime -> String
prettyTime = formatTime defaultTimeLocale "%B %e, %Y %r"

-- Set up i18n messages. See the message folder.
mkMessage "App" "messages" "en"

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the linked documentation for an
-- explanation for this split.
mkYesodData "App" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    approot = ApprootMaster $ appRoot . settings

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = fmap Just $ defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            $(combineScripts 'StaticR
                 [ js_jquery_1_11_1_min_js
                 , js_jquery_ui_1_11_2_js
                 , js_bootstrap_3_2_0_js
                 , js_moment_2_8_3_min_js
                 , js_bootstrap_table_min_js
                 , kalendae_kalendae_standalone_js
                 -- , js_html5shiv_3_7_4_min_js
                 -- , js_respond_1_4_2_min_js
                 ])

            $(combineStylesheets 'StaticR
                [ css_jquery_ui_1_11_2_min_css
                , css_bootstrap_3_2_0_css
                , css_bootstrap_theme_3_2_0_css
                , css_bootstrap_table_min_css
                , kalendae_kalendae_css
                ])

            widget

        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticRoot setting in Settings.hs
    urlRenderOverride y (StaticR s) =
        Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $ renderRoute s
    urlRenderOverride _ _ = Nothing

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- Routes not requiring authenitcation.
    isAuthorized (AuthR _) _ = return Authorized
    isAuthorized FaviconR _ = return Authorized
    isAuthorized HomeR _ = return Authorized
    isAuthorized DayBookingStatusR _ = return Authorized
                                  
    -- Default to Authorized for now.
    isAuthorized _ _ = return Authorized  --return $ Unauthorized "请先登录" -- return Authorized 

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent =
        addStaticContentExternal minifym genFileName Settings.staticDir (StaticR . flip StaticRoute [])
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs
            | development = "autogen-" ++ base64md5 lbs
            | otherwise   = base64md5 lbs

    -- Place Javascript at bottom of the body tag so the rest of the page loads first
    jsLoader _ = BottomOfBody

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog _ _source level =
        development || level == LevelWarn || level == LevelError

    makeLogger = return . appLogger

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB = defaultRunDB persistConfig connPool
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner connPool

instance YesodAuth App where
    type AuthId App = Text
    loginDest _ = HomeR
    logoutDest _ = HomeR
    authPlugins _ = [authConf]
    authHttpManager _ = error "No manager needed"
    onLogin = return ()
    getAuthId = return . Just . credsIdent
    maybeAuthId = lookupSession credsKey -- "__ID"

-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

instance YesodJquery App where
    urlJqueryJs _ = Left $ StaticR js_jquery_1_11_1_min_js
    urlJqueryUiJs _ = Left $ StaticR js_jquery_ui_1_11_2_js
    urlJqueryUiCss _ = Left $ StaticR css_jquery_ui_1_11_2_min_css
    urlJqueryUiDateTimePicker _ = Right "http://github.com/gregwebs/jquery.ui.datetimepicker/raw/master/jquery.ui.datetimepicker.js"

-- | Get the 'Extra' value, used to hold data from the settings.yml file.
getExtra :: Handler Extra
getExtra = fmap (appExtra . settings) getYesod

-- Note: previous versions of the scaffolding included a deliver function to
-- send emails. Unfortunately, there are too many different options for us to
-- give a reasonable default. Instead, the information is available on the
-- wiki:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email

------------------------------------------------------------------------------------------
---- auth interface
reloadR = PluginR "authConf" ["reload"]

authConf :: AuthPlugin App
authConf = AuthPlugin "authConf" dispatch login
    where
    dispatch "POST" ["login"] = do
        emailName <- lift $ runInputPost $ ireq textField "邮箱"
        pw <- lift $ runInputPost $ ireq textField "密码"
        mayUser <- lift $ runDB $ verifyUserWithPassword emailName pw
        case mayUser of
            Nothing -> lift $ redirect (AuthR reloadR)  
            Just aUser -> do                                
                lift $ setCreds False $ Creds "email" emailName [(emailName, userName aUser)]
                if userLevel aUser == AuthAdmin
                   then lift $ redirect ManageR
                   else lift $ redirect HomeR
                
    dispatch "GET" ["reload"] = getReloadR >>= sendResponse
    dispatch _ _ = notFound

    url = PluginR "authConf" ["login"]
    login authToMaster = loginWidget authToMaster url

getReloadR :: AuthHandler App Html
getReloadR =  lift $ defaultLayout $ do
    [whamlet|
        <h4> 无效的用户名或密码
        <p>请重新 
            <a href=@{AuthR LoginR}>登入
    |]
------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
-- other helpers
doAuthAndGetUserInfo = do
    maid <- maybeAuthId
    maybeUserInfo <- do
        case maid of 
            Nothing -> return Nothing
            Just theEmail -> runDB $ getBy $ UniqueEmail theEmail
    return maybeUserInfo

verifyUserWithPassword theEmail thePassword = do
    mayInfo <- getBy $ UniqueEmail theEmail
    liftIO $ print theEmail
    liftIO $ print thePassword 
    case mayInfo of	    
        Nothing -> do   
                   curTime <- liftIO $ getCurrentTime		
		           -- we set a default admin user: pekingphyconfadmin@163.com - pekingphyconfadmin as password.
                   if theEmail == "pekingphyconfadmin@163.com" && thePassword == "pekingphyconfadmin"
                      then do 
                           let firstAdmin = User { userEmail = "pekingphyconfadmin@163.com",	
                                                   userPassword = "pekingphyconfadmin",
                                                   userName = "系统管理员",
                                                   userLevel = AuthAdmin,
                                                   userFirstAdd = curTime}
                           insert firstAdmin
                           return $ Just firstAdmin					   
                      else (liftIO $ print "invalid username") >> return Nothing
        Just (Entity _ aUser) -> do
            if userPassword aUser == thePassword
               then return . Just $ aUser
               else do
                    liftIO $ print $ "invalid password for " ++ (show $ userName aUser)
                    return Nothing

loginWidget authToMaster url = toWidget $ [hamlet|
$newline never
<div class="row">
    <form class="form-horizontal control-label" method="post" action="@{authToMaster url}">
        <div class="col-md-12" align="left">
            <h4> 会议室预定系统登入: #
        <div class="col-md-12" align="left">
            <br>
        <div class="col-md-12" align="left">
            <br>
        <div class="col-md-2" align="left">
            <lable for="useremail">邮箱
        <div class="col-md-10" align="left">
            <input type="text" name="邮箱" id="useremail">
        <div class="col-md-12" align="left">
            <br>
        <div class="col-md-2" align="left">
            <lable for="userpassword">密码
        <div class="col-md-10" align="left">
            <input type="password" name="密码" id="userpassword">
        <div class="col-md-12" align="left">
            <br>
        <div class="col-md-12" align="left">
            <br>
        <div class="col-md-1" align="left" >
            <input class="btn btn-info" type="submit" value="登入">
        <div class="col-md-1" align="left">
            <input class="btn btn-primary" type="button" value="返回" onclick="window.location.href='@{HomeR}'">

|]