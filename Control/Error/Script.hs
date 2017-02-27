{-# LANGUAGE OverloadedStrings #-}

{-|
    Use this module if you like to write simple scripts with 'Text'-based
    errors, but you prefer to use 'ExceptT' to handle errors rather than
    @Control.Exception@.

> import Control.Error
>
> main = runScript $ do
>     str <- scriptIO getLine
>     n   <- tryRead "Read failed" str
>     scriptIO $ print (n + 1)
-}

module Control.Error.Script (
    -- * The Script Monad
    Script,
    runScript,
    scriptIO
    ) where

import Control.Exception (try, SomeException)
import Control.Monad (liftM)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Except (ExceptT(ExceptT), runExceptT)
import Control.Error.Util (errLn)
import Data.EitherR (fmapL)
import Data.Monoid ((<>))
import Data.Text (Text)
import System.Environment (getProgName)
import System.Exit (exitFailure)

-- Documentation
import Control.Monad.Trans.Class (lift)
import System.IO (stderr)

import qualified Data.Text

-- | An 'IO' action that can fail with a 'Text' error message
type Script = ExceptT Text IO

{-| Runs the 'Script' monad

    Prints the first error to 'stderr' and exits with 'exitFailure'
-}
runScript :: Script a -> IO a
runScript s = do
    e <- runExceptT s
    case e of
        Left  e -> do
            let adapt str = Data.Text.pack str <> ": " <> e
            errLn =<< liftM adapt getProgName
            exitFailure
        Right a -> return a

{-| 'scriptIO' resembles 'lift', except it catches all exceptions and converts
    them to 'Text'

    Note that 'scriptIO' is compatible with the 'Script' monad.
-}
scriptIO :: (MonadIO m) => IO a -> ExceptT Text m a
scriptIO = ExceptT
         . liftIO
         . liftM (fmapL (Data.Text.pack . show))
         . (try :: IO a -> IO (Either SomeException a))
