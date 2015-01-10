{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module LambdaBenchmark.Employee
       ( module Export
       )where

import LambdaBenchmark.Employee.Import
import LambdaBenchmark.Employee.Foundation        as Export
import LambdaBenchmark.Employee.Models            as Export
import LambdaBenchmark.Employee.Handler.Employee   as Export

instance LambdaBenchmarkEmployee master => YesodSubDispatch EmployeeAdmin (HandlerT master IO) where
  yesodSubDispatch = $(mkYesodSubDispatch resourcesEmployeeAdmin)
