{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

module LambdaBenchmark.Project.Foundation where

import           Yesod
import           Data.Text                 (Text)

import           LambdaCms.Core

import           LambdaBenchmark.Project.Message (ProjectMessage, defaultMessage, englishMessage)
import qualified LambdaBenchmark.Project.Message as Msg
import           LambdaBenchmark.Project.Models

data ProjectAdmin = ProjectAdmin

mkYesodSubData "ProjectAdmin" $(parseRoutesFile "config/routes")

instance LambdaBenchmarkProject master => RenderMessage master ProjectMessage where
    renderMessage = renderProjectMessage

type ProjectHandler a = forall master. LambdaBenchmarkProject master => HandlerT ProjectAdmin (HandlerT master IO) a

type ProjectForm x = forall master. LambdaBenchmarkProject master => Html -> MForm (HandlerT master IO) (FormResult x, WidgetT master IO ())

class LambdaCmsAdmin master => LambdaBenchmarkProject master where
    projectR :: Route ProjectAdmin -> Route master

    renderProjectMessage :: master
                       -> [Text]
                       -> ProjectMessage
                       -> Text
    renderProjectMessage m (lang:langs) = do
        case (lang `elem` (renderLanguages m), lang) of
            (True, "en") -> englishMessage
            _ -> renderProjectMessage m langs
    renderProjectMessage _ _ = defaultMessage

defaultProjectAdminMenu :: LambdaBenchmarkProject master => (Route ProjectAdmin -> Route master) -> [AdminMenuItem master]
defaultProjectAdminMenu tp = [ MenuItem (SomeMessage Msg.MenuProject) (tp ProjectAdminIndexR) "pushpin" ]
