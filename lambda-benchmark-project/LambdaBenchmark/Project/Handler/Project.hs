{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module LambdaBenchmark.Project.Handler.Project
    ( getProjectAdminIndexR
    , getProjectAdminNewR
    , postProjectAdminNewR
    , getProjectAdminEditR
    , patchProjectAdminEditR
    , deleteProjectAdminEditR
    ) where

import           Data.Maybe              (fromJust, isJust, fromMaybe)
import           Data.Time               (UTCTime, getCurrentTime, utctDay)
import           LambdaCms.Core.Settings
import           LambdaBenchmark.Project.Import
import qualified LambdaBenchmark.Project.Message as Msg

getProjectAdminIndexR    :: ProjectHandler Html
getProjectAdminNewR      :: ProjectHandler Html
postProjectAdminNewR     :: ProjectHandler Html
getProjectAdminEditR     :: ProjectId -> ProjectHandler Html
patchProjectAdminEditR   :: ProjectId -> ProjectHandler Html
deleteProjectAdminEditR  :: ProjectId -> ProjectHandler Html

getProjectAdminIndexR = lift $ do
    can <- getCan
    (projects :: [Entity Project]) <- runDB $ selectList [] []
    adminLayout $ do
        setTitleI Msg.ProjectIndex
        $(widgetFile "index")

getProjectAdminNewR = lift $ do
    can <- getCan
    ct <- liftIO getCurrentTime
    (fWidget, enctype) <- generateFormPost $ projectForm Nothing ct
    adminLayout $ do
        setTitleI Msg.NewProject
        $(widgetFile "new")

postProjectAdminNewR = do
    ct <- liftIO getCurrentTime
    ((results, fWidget), enctype) <- lift . runFormPost $ projectForm Nothing ct
    case results of
        FormSuccess project -> do
            _ <- lift . runDB $ insert project
            lift $ setMessageI Msg.SaveSuccess
            redirect ProjectAdminIndexR
        _ -> lift $ do
            can <- getCan
            adminLayout $ do
                setTitleI Msg.NewProject
                $(widgetFile "new")

getProjectAdminEditR projectId = lift $ do
    project <- runDB $ get404 projectId
    can <- getCan
    ct <- liftIO getCurrentTime
    (fWidget, enctype) <- generateFormPost $ projectForm (Just project) ct
    adminLayout $ do
        setTitleI Msg.EditProject
        $(widgetFile "edit")

patchProjectAdminEditR projectId = do
    project <- lift . runDB $ get404 projectId
    ct <- liftIO getCurrentTime
    ((results, fWidget), enctype) <- lift . runFormPost $ projectForm (Just project) ct
    case results of
        FormSuccess newProject -> do
            lift $ runDB $ replace projectId newProject
            lift $ setMessageI Msg.UpdateSuccess
            redirect $ ProjectAdminEditR projectId
        _ -> lift $ do
            can <- getCan
            adminLayout $ do
                setTitleI Msg.EditProject
                $(widgetFile "edit")

deleteProjectAdminEditR projectId = do
    project <- lift . runDB $ get404 projectId
    lift . runDB $ delete projectId
    lift $ setMessageI Msg.DeleteSuccess
    redirect ProjectAdminIndexR

projectForm :: Maybe Project -> UTCTime -> ProjectForm Project
projectForm mProject utct = renderBootstrap3 BootstrapBasicForm $ Project
    <$> areq textField (bfs Msg.Ident) (projectIdent <$> mProject)
    <*> areq textField (bfs Msg.Title) (projectTitle <$> mProject)
    <*> areq textareaField (bfs Msg.Content) (projectContent <$> mProject)
    <*> areq textField (bfs Msg.Url) (projectUrl <$> mProject)
    <*> areq textField (bfs Msg.Customer) (projectCustomer <$> mProject)
    <*> areq textField (bfs Msg.Lang) (projectLang <$> mProject)
    <*> pure (fromMaybe utct $ projectCreatedAt <$> mProject)
    <*  bootstrapSubmit (BootstrapSubmit Msg.Save " btn-success " [])
