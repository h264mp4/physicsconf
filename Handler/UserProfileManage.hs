{-# LANGUAGE TupleSections, OverloadedStrings, BangPatterns #-}
module Handler.UserProfileManage where

import Import
import Yesod.Auth
import Yesod.Form.Bootstrap3 
import Text.Julius(rawJS)

import Data.Text.Encoding(encodeUtf8)
import Data.ByteString.Lazy(fromStrict)
import Data.Maybe(fromJust,isJust)
import Data.Aeson(ToJSON(..), object, (.=), decode)
import Data.Conduit
import qualified Data.Conduit.Text as CT
import qualified Data.Conduit.List as CL
import qualified Data.Text(pack)
import qualified Data.Text as T

import Handler.DBOperation
import Handler.MiscTypes
import Handler.Utils

import CommonWidget

------------------------------------------------------------------------------------------
---- User Rrcord Manage.

getUserProfileManageR :: Handler Html
getUserProfileManageR = do
    -- we do auth first
    maid <- maybeAuthId
    case maid of 
        Nothing -> redirect (AuthR LoginR)
        Just theEmail -> do
            maybeUserInfo <- runDB $ getBy $ UniqueEmail theEmail
            case maybeUserInfo of
                Nothing -> notFound
                Just (Entity theUserId theUser) -> do
                    let uname = userName theUser
                        createdTime = T.pack . show . convertUtcToZoneTime . userFirstAdd $ theUser
                        maybeUser = Just theUser
                    (userProfileForm, formEnctype) <- 
                                      generateFormPost $ userProfileMangeForm theUser         
                    defaultLayout $ do
                        toWidget $(widgetFile "userprofilemanage")

postUserProfileManageR :: Handler Html
postUserProfileManageR = do
    maid <- maybeAuthId
    case maid of 
        Nothing -> redirect (AuthR LoginR)
        Just theEmail -> do
            maybeUserInfo <- runDB $ getBy $ UniqueEmail theEmail
            case maybeUserInfo of
                Nothing -> notFound
                Just (Entity theUserId theUser) -> do
                    ((result, formWidget), formEnctype) <- 
                                              runFormPost $ userProfileMangeForm theUser
                    liftIO $ print result
                    case result of
                        FormSuccess formInfo -> do
                            if userEmail formInfo /= userEmail theUser
                               then notFound
                               else do
                                    runDB $ updateUserProfile theUserId formInfo
                                    defaultLayout $ do
                                        backNavWidget emptyText ("已保存"::Text) HomeR
                        _ -> defaultLayout $ do
                                 backNavWidget emptyText ("无效信息, 请重新输入"::Text) 
                                               UserProfileManageR

userProfileMangeForm :: User -> Form User
userProfileMangeForm aUser = renderBootstrap3 commonSimpleFormLayout $ 
    User
        <$> pure (userEmail aUser)
        <*> areq passwordConfirmField "新密码" Nothing
        <*> areq textField "姓名" (Just $ userName aUser)            
        <*> pure (userLevel aUser)
        <*> pure (userFirstAdd aUser)

passwordConfirmField :: Field Handler Text
passwordConfirmField = Field
    { fieldParse = \rawVals _fileVals ->
        case rawVals of
            [a, b]
                | a == b -> return $ Right $ Just a
                | otherwise -> return $ Left "Passwords don't match"
            [] -> return $ Right Nothing
            _ -> return $ Left "You must enter two values"
    , fieldView = \idAttr nameAttr otherAttrs eResult isReq ->
        [whamlet|
            <input id=#{idAttr} name=#{nameAttr} *{otherAttrs} type=password>
            <div>确认:
            <input id=#{idAttr}-confirm name=#{nameAttr} *{otherAttrs} type=password>
        |]
    , fieldEnctype = UrlEncoded
    }