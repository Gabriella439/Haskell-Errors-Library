{-|
    Import this module in your code to access the entire library's
    functionality:

> import Control.Error

    This module exports the entire library as well as functions from the @Safe@
    library.
-}

module Control.Error (
    module Control.Error.Script,
    module Control.Error.Safe,
    module Control.Error.Util,
    module Data.EitherR,
    module Safe
    ) where

import Control.Error.Script
import Control.Error.Safe
import Control.Error.Util
import Data.EitherR
import Safe
