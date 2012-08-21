{-|
    Use this module if you like to write simple scripts, but you prefer to use
    'EitherT' to handle errors rather than @Control.Exception@.

> import Control.Error
>
> main = runScript $ do
>     str <- tryIO getLine
>     n   <- tryRead "Read failed" str
>     tryIO $ print (n + 1)
-}

module Control.Error.Script (
    -- * The Script Monad
    Script,
    runScript,
    tryMaybe,
    tryEither,
    tryIO
    ) where

import Control.Exception
import Control.Monad.Trans.Either
import Control.Error.Util
import Data.EitherR
import System.IO
import System.Exit

-- | An 'IO' action that can fail with a 'String' error message
type Script = EitherT String IO

{-|
    Runs the 'Script' monad

    Prints the first error to 'stderr' and exits with 'exitFailure'
-}
runScript :: Script a -> IO a
runScript s = do
    e <- runEitherT s
    case e of
        Left  e -> do
            hPutStrLn stderr e
            exitFailure
        Right a -> return a

-- | A 'Maybe' that fails in the 'Script' monad
tryMaybe :: String -> Maybe a -> Script a
tryMaybe str = tryEither . note str

-- | An 'Either' that fails in the 'Script' monad
tryEither :: Either String r -> Script r
tryEither = hoistEither

-- | 'tryIO' is like 'lift', except it converts exceptions to the 'Script' monad
tryIO :: IO a -> Script a
tryIO io = EitherT . fmap (fmapL show)
                   $ (try :: IO a -> IO (Either SomeException a)) io
