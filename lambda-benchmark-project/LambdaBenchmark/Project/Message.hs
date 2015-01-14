{-# LANGUAGE OverloadedStrings #-}

module LambdaBenchmark.Project.Message
       ( ProjectMessage (..)
       , defaultMessage
         -- * All languages
       , englishMessage
       ) where

import           Data.Monoid ((<>))
import           Data.Text   (Text)

data ProjectMessage =
    MenuProject
  | ProjectIndex
  | NewProject
  | EditProject
  | SaveSuccess
  | UpdateSuccess
  | DeleteSuccess
  | Title
  | Save
  | Back
  | Delete
  | CreatedOn
  | ChangeProjectSettings
  | NoProjectsFound
  | Content
  | Url
  | Customer
  | Lang
  | Ident

defaultMessage :: ProjectMessage -> Text
defaultMessage = englishMessage

englishMessage :: ProjectMessage -> Text
englishMessage MenuProject           = "Projects"
englishMessage ProjectIndex          = "Project overview"
englishMessage NewProject            = "New project"
englishMessage EditProject           = "Edit project"
englishMessage SaveSuccess           = "Successfully saved"
englishMessage UpdateSuccess         = "Successfully updated"
englishMessage DeleteSuccess         = "Successfully deleted"
englishMessage Title                 = "Title"
englishMessage Save                  = "Save"
englishMessage Back                  = "Back"
englishMessage Delete                = "Delete"
englishMessage CreatedOn             = "Created on"
englishMessage ChangeProjectSettings = "Change project settings"
englishMessage NoProjectsFound       = "No projects found"
englishMessage Content               = "Content"
englishMessage Url               = "Url"
englishMessage Customer               = "Customer"
englishMessage Lang               = "Language"
englishMessage Ident               = "Translation id"
