{-# LANGUAGE TupleSections, OverloadedStrings, BangPatterns #-}
module Handler.Manage where

import Import
import Yesod.Auth
import Database.Persist.Sql

import qualified Data.Text as T

import Handler.MiscTypes
import Handler.Utils
import Handler.DBOperation
import CommonWidget

------------------------------------------------------------------------------------------
---- Admin Manage Page
getManageR :: Handler Html
getManageR =  do 
    maybeUserInfo <- doAuthAndGetUserInfo
    let maybeAdminUser = case maybeUserInfo of
                             Nothing -> Nothing
                             Just (Entity aId aUser) -> Just aUser    
    
    let addLink = AddRoomR 
        listLink = ListRoomR 
        editLink = EditRoomR 
        deleteLink = DeleteRoomR 
        dataType = ("typeroom"::Text) 
        buttonName = ("新建会议室":: Text) 
        addNew = True
        -- make compiler happy
        userInfos = []
        theEmailStr = emptyText
    defaultLayout $ do
        aRandomTableId <- newIdent
        userListTableId <- newIdent
        manageBookingClass <- newIdent
        toWidget $(widgetFile "manage")

getManageUserR :: Handler Html
getManageUserR = do
    maybeUserInfo <- doAuthAndGetUserInfo
    let maybeAdminUser = case maybeUserInfo of
                             Nothing -> Nothing
                             Just (Entity _ aUser) -> Just aUser

    let addLink = AddUserR 
        listLink = ListUserR
        editLink = EditUserR 
        deleteLink = DeleteUserR 
        dataType = ("typeuser"::Text) 
        buttonName = ("新建用户":: Text) 
        addNew = True
        userInfos = []
        theEmailStr = emptyText
    defaultLayout $ do
        aRandomTableId <- newIdent
        userListTableId <- newIdent
        manageBookingClass <- newIdent
        $(widgetFile "manage")

-- listLink editLink deleteLink
getManageRoomR :: Handler Html
getManageRoomR = do
    maybeUserInfo <- doAuthAndGetUserInfo
    let maybeAdminUser = case maybeUserInfo of
                             Nothing -> Nothing
                             Just (Entity _ aUser) -> Just aUser

    let addLink = AddRoomR 
        listLink = ListRoomR
        editLink = EditRoomR 
        deleteLink = DeleteRoomR 
        dataType = ("typeroom"::Text) 
        buttonName = ("新建会议室":: Text) 
        addNew = True
        userInfos = []
        theEmailStr = emptyText
    defaultLayout $ do
        aRandomTableId <- newIdent
        userListTableId <- newIdent
        manageBookingClass <- newIdent
        $(widgetFile "manage")

-- list all users
getManageAllUserBookingR :: Handler Html
getManageAllUserBookingR = do
    maybeUserInfo <- doAuthAndGetUserInfo
    let maybeAdminUser = case maybeUserInfo of
                             Nothing -> Nothing
                             Just (Entity _ aUser) -> Just aUser

    -- if we check authLevel here, we could avoid a lot of DB operations.
    -- but for now, we just leave it.
    
    -- it is impossible to be empty, at least include the admin.    
    allUsers <- runDB $ listUserProfile
    let userInfos = map (\(Entity _ aUser) -> aUser) allUsers
    mayEmailStr <- lookupGetParam "bookingInfoByUserEmail"
    let theEmailStr = case mayEmailStr of
                          Nothing -> userEmail (userInfos !! 0)
                          Just userEmailStr -> userEmailStr

    defaultLayout $ do
        let addLink = AddUserR 
            listLink = ListUserR
            editLink = EditUserR 
            deleteLink = DeleteUserR 
            dataType = ("typeuser"::Text) 
            buttonName = ("新建用户":: Text) 
            addNew = False
        aRandomTableId <- newIdent
        userListTableId <- newIdent
        manageBookingClass <- newIdent
        $(widgetFile "manage")