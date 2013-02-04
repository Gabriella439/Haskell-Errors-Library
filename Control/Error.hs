{-|
    Import this module in your code to access the entire library's
    functionality:

> import Control.Error

    This module exports the entire library as well as useful exports from other
    standard error-handling libraries:

    * "Control.Error.Safe": Generalizes the @safe@ library, including 'Either',
      'EitherT', and 'MonadPlus' variations on total functions

    * "Control.Error.Script": Support for simple scripts that catch all errors
      and transform them to 'String's

    * "Control.Error.Util": Utility functions and conversions between common
      error-handling types

    * @Control.Monad.Trans.Either@: The 'EitherT' monad transformer

    * @Control.Monad.Trans.Maybe@: The 'MaybeT' monad transformer

    * @Data.Either@: 'Either' utility functions

    * "Data.EitherR": throw and catch functions, and their corresponding
      \"success\" monads

    * @Data.Maybe@: 'Maybe' utility functions

    * @Safe@: Total versions of partial Prelude functions

    This module does not re-export partial functions from other libraries.
-}

module Control.Error (
    module Control.Error.Safe,
    module Control.Error.Script,
    module Control.Error.Util,
    module Control.Monad.Trans.Either,
    module Control.Monad.Trans.Maybe,
    module Data.Either,
    module Data.EitherR,
    module Data.Maybe,
    module Safe
    ) where

import Control.Error.Safe
import Control.Error.Script
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
