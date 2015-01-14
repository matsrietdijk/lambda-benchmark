module Handler.Home where

import Import

import LambdaBenchmark.Employee
import LambdaBenchmark.Project

import LambdaCms.Core

getHomeR :: Handler Html
getHomeR = do
    y <- getYesod
    let supportedLangs = renderLanguages y
    mlang <- lookupGetParam "lang"
    let lang = case mlang of
            Just l -> case l `elem` supportedLangs of
                True -> l
                False -> "en"
            Nothing -> "en"

    setLanguage lang
    projects <- runDB $ selectList [ProjectLang ==. lang] [Desc ProjectId, LimitTo 5]
    employees <- runDB $ selectList [EmployeeLang ==. lang] [Asc EmployeeTitle]
    defaultLayout $ do
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")
