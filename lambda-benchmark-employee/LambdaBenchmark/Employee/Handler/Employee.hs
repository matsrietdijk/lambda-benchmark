{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module LambdaBenchmark.Employee.Handler.Employee
    ( getEmployeeAdminIndexR
    , getEmployeeAdminNewR
    , postEmployeeAdminNewR
    , getEmployeeAdminEditR
    , patchEmployeeAdminEditR
    , deleteEmployeeAdminEditR
    ) where

import           Data.Maybe              (fromJust, isJust, fromMaybe)
import           Data.Time               (UTCTime, getCurrentTime, utctDay)
import           LambdaCms.Core.Settings
import           LambdaBenchmark.Employee.Import
import qualified LambdaBenchmark.Employee.Message as Msg

getEmployeeAdminIndexR    :: EmployeeHandler Html
getEmployeeAdminNewR      :: EmployeeHandler Html
postEmployeeAdminNewR     :: EmployeeHandler Html
getEmployeeAdminEditR     :: EmployeeId -> EmployeeHandler Html
patchEmployeeAdminEditR   :: EmployeeId -> EmployeeHandler Html
deleteEmployeeAdminEditR  :: EmployeeId -> EmployeeHandler Html

getEmployeeAdminIndexR = lift $ do
    can <- getCan
    (employees :: [Entity Employee]) <- runDB $ selectList [] []
    adminLayout $ do
        setTitleI Msg.EmployeeIndex
        $(widgetFile "index")

getEmployeeAdminNewR = lift $ do
    can <- getCan
    ct <- liftIO getCurrentTime
    (fWidget, enctype) <- generateFormPost $ employeeForm Nothing ct
    adminLayout $ do
        setTitleI Msg.NewEmployee
        $(widgetFile "new")

postEmployeeAdminNewR = do
    ct <- liftIO getCurrentTime
    ((results, fWidget), enctype) <- lift . runFormPost $ employeeForm Nothing ct
    case results of
        FormSuccess employee -> do
            _ <- lift . runDB $ insert employee
            lift $ setMessageI Msg.SaveSuccess
            redirect EmployeeAdminIndexR
        _ -> lift $ do
            can <- getCan
            adminLayout $ do
                setTitleI Msg.NewEmployee
                $(widgetFile "new")

getEmployeeAdminEditR employeeId = lift $ do
    employee <- runDB $ get404 employeeId
    can <- getCan
    ct <- liftIO getCurrentTime
    (fWidget, enctype) <- generateFormPost $ employeeForm (Just employee) ct
    adminLayout $ do
        setTitleI Msg.EditEmployee
        $(widgetFile "edit")

patchEmployeeAdminEditR employeeId = do
    employee <- lift . runDB $ get404 employeeId
    ct <- liftIO getCurrentTime
    ((results, fWidget), enctype) <- lift . runFormPost $ employeeForm (Just employee) ct
    case results of
        FormSuccess newEmployee -> do
            lift $ runDB $ replace employeeId newEmployee
            lift $ setMessageI Msg.UpdateSuccess
            redirect $ EmployeeAdminEditR employeeId
        _ -> lift $ do
            can <- getCan
            adminLayout $ do
                setTitleI Msg.EditEmployee
                $(widgetFile "edit")

deleteEmployeeAdminEditR employeeId = do
    employee <- lift . runDB $ get404 employeeId
    lift . runDB $ delete employeeId
    lift $ setMessageI Msg.DeleteSuccess
    redirect EmployeeAdminIndexR

employeeForm :: Maybe Employee -> UTCTime -> EmployeeForm Employee
employeeForm mEmployee utct = renderBootstrap3 BootstrapBasicForm $ Employee
    <$> areq textField (bfs Msg.Title) (employeeTitle <$> mEmployee)
    <*> areq textareaField (bfs Msg.Content) (employeeContent <$> mEmployee)
    <*> areq textField (bfs Msg.Firstname) (employeeFirstname <$> mEmployee)
    <*> areq textField (bfs Msg.Lastname) (employeeLastname <$> mEmployee)
    <*> pure (fromMaybe utct $ employeeCreatedAt <$> mEmployee)
    <*  bootstrapSubmit (BootstrapSubmit Msg.Save " btn-success " [])
