{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

module LambdaBenchmark.Employee.Foundation where

import           Yesod
import           Data.Text                 (Text)

import           LambdaCms.Core

import           LambdaBenchmark.Employee.Message (EmployeeMessage, defaultMessage, englishMessage)
import qualified LambdaBenchmark.Employee.Message as Msg
import           LambdaBenchmark.Employee.Models

data EmployeeAdmin = EmployeeAdmin

mkYesodSubData "EmployeeAdmin" $(parseRoutesFile "config/routes")

instance LambdaBenchmarkEmployee master => RenderMessage master EmployeeMessage where
    renderMessage = renderEmployeeMessage

type EmployeeHandler a = forall master. LambdaBenchmarkEmployee master => HandlerT EmployeeAdmin (HandlerT master IO) a

type EmployeeForm x = forall master. LambdaBenchmarkEmployee master => Html -> MForm (HandlerT master IO) (FormResult x, WidgetT master IO ())

class LambdaCmsAdmin master => LambdaBenchmarkEmployee master where
    employeeR :: Route EmployeeAdmin -> Route master

    renderEmployeeMessage :: master
                       -> [Text]
                       -> EmployeeMessage
                       -> Text
    renderEmployeeMessage m (lang:langs) = do
        case (lang `elem` (renderLanguages m), lang) of
            (True, "en") -> englishMessage
            _ -> renderEmployeeMessage m langs
    renderEmployeeMessage _ _ = defaultMessage

defaultEmployeeAdminMenu :: LambdaBenchmarkEmployee master => (Route EmployeeAdmin -> Route master) -> [AdminMenuItem master]
defaultEmployeeAdminMenu tp = [ MenuItem (SomeMessage Msg.MenuEmployee) (tp EmployeeAdminIndexR) "pushpin" ]
