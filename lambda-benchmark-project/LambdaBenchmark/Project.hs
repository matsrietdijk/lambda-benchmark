{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module LambdaBenchmark.Project
       ( module Export
       )where

import LambdaBenchmark.Project.Import
import LambdaBenchmark.Project.Foundation        as Export
import LambdaBenchmark.Project.Models            as Export
import LambdaBenchmark.Project.Handler.Project   as Export

instance LambdaBenchmarkProject master => YesodSubDispatch ProjectAdmin (HandlerT master IO) where
  yesodSubDispatch = $(mkYesodSubDispatch resourcesProjectAdmin)
