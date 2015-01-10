{-# LANGUAGE OverloadedStrings #-}

module LambdaBenchmark.Employee.Message
       ( EmployeeMessage (..)
       , defaultMessage
         -- * All languages
       , englishMessage
       ) where

import           Data.Monoid ((<>))
import           Data.Text   (Text)

data EmployeeMessage =
    MenuEmployee
  | EmployeeIndex
  | NewEmployee
  | EditEmployee
  | SaveSuccess
  | UpdateSuccess
  | DeleteSuccess
  | Title
  | Save
  | Back
  | Delete
  | CreatedOn
  | ChangeEmployeeSettings
  | NoEmployeesFound

defaultMessage :: EmployeeMessage -> Text
defaultMessage = englishMessage

englishMessage :: EmployeeMessage -> Text
englishMessage MenuEmployee           = "Employees"
englishMessage EmployeeIndex          = "Employee overview"
englishMessage NewEmployee            = "New employee"
englishMessage EditEmployee           = "Edit employee"
englishMessage SaveSuccess           = "Successfully saved"
englishMessage UpdateSuccess         = "Successfully updated"
englishMessage DeleteSuccess         = "Successfully deleted"
englishMessage Title                 = "Title"
englishMessage Save                  = "Save"
englishMessage Back                  = "Back"
englishMessage Delete                = "Delete"
englishMessage CreatedOn             = "Created on"
englishMessage ChangeEmployeeSettings = "Change employee settings"
englishMessage NoEmployeesFound       = "No employees found"
