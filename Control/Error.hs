{-|
    Import this module in your code to access the entire library's
    functionality:

> import Control.Error

    This module exports the entire library as well as useful exports from other
    standard error-handling libraries.

    This module does not re-export partial functions from other libraries.
-}

module Control.Error (
    module Control.Error.Script,
    module Control.Error.Safe,
    module Control.Error.Util,
    module Control.Monad.Trans.Either,
    module Control.Monad.Trans.Maybe,
    module Data.Either,
    module Data.EitherR,
    module Data.Maybe,
    module Safe
    ) where

import Control.Error.Script
import Control.Error.Safe
import Control.Error.Util
import Control.Monad.Trans.Either
import Control.Monad.Trans.Maybe
import Data.Either
import Data.EitherR
import Data.Maybe hiding (fromJust)
import Safe hiding (
    tailNote,
    initNote,
    headNote,
    lastNote,
    minimumNote,
    maximumNote,
    foldr1Note,
    foldl1Note,
    foldl1Note',
    fromJustNote,
    assertNote,
    at,
    atNote,
    readNote,
    lookupJust,
    lookupJustNote,
    findJust,
    findJustNote,
    abort)
