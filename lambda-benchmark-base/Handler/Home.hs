module Handler.Home where

import Import

import LambdaBenchmark.Employee
import LambdaBenchmark.Project

getHomeR :: Handler Html
getHomeR = do
    projects <- runDB $ selectList [] [Desc ProjectId, LimitTo 5]
    employees <- runDB $ selectList [] [Asc EmployeeTitle]
    defaultLayout $ do
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")
