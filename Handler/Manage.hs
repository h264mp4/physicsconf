{-# LANGUAGE TupleSections, OverloadedStrings, BangPatterns #-}
module Handler.Manage where

import Import
import Yesod.Auth

import Handler.MiscTypes
import Handler.DBOperation
import CommonWidget

------------------------------------------------------------------------------------------
---- Admin Manage Page
getManageR :: Handler Html
getManageR =  do 
    maybeUserInfo <- doAuthAndGetUserInfo
    let maybeUser = case maybeUserInfo of
                        Nothing -> Nothing
                        Just (Entity _ aUser) -> Just aUser

    let addLink = AddRoomR 
        listLink = ListRoomR 
        editLink = EditRoomR 
        deleteLink = DeleteRoomR 
        dataType = ("typeroom"::Text) 
        buttonName = ("新建会议室":: Text) 

    defaultLayout $ do
        aRandomTableId <- newIdent
        toWidget $(widgetFile "manage")

getManageUserR :: Handler Html
getManageUserR = do
    maybeUserInfo <- doAuthAndGetUserInfo
    let maybeUser = case maybeUserInfo of
                        Nothing -> Nothing
                        Just (Entity _ aUser) -> Just aUser

    let addLink = AddUserR 
        listLink = ListUserR
        editLink = EditUserR 
        deleteLink = DeleteUserR 
        dataType = ("typeuser"::Text) 
        buttonName = ("新建用户":: Text) 
    defaultLayout $ do
        aRandomTableId <- newIdent
        $(widgetFile "manage")

-- listLink editLink deleteLink
getManageRoomR :: Handler Html
getManageRoomR = do
    maybeUserInfo <- doAuthAndGetUserInfo
    let maybeUser = case maybeUserInfo of
                        Nothing -> Nothing
                        Just (Entity _ aUser) -> Just aUser

    let addLink = AddRoomR 
        listLink = ListRoomR
        editLink = EditRoomR 
        deleteLink = DeleteRoomR 
        dataType = ("typeroom"::Text) 
        buttonName = ("新建会议室":: Text) 
    defaultLayout $ do
        aRandomTableId <- newIdent
        $(widgetFile "manage")

