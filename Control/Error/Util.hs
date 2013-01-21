-- | This module exports miscellaneous error-handling functions.

module Control.Error.Util (
    -- * Conversion
    -- $conversion
    hush,
    hushT,
    note,
    noteT,
    hoistMaybe,
    -- * MaybeT
    maybeT,
    -- * Either
    isLeft,
    isRight,
    fmapR,
    -- * EitherT
    fmapRT,
    -- * Error Reporting
    err,
    errLn,
    -- * Exceptions
    tryIO
    ) where

import Control.Exception (try, IOException)
import Control.Monad (liftM)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Either (EitherT(EitherT, runEitherT))
import Control.Monad.Trans.Maybe (MaybeT(MaybeT, runMaybeT))
import System.IO (hPutStr, hPutStrLn, stderr)

-- For Documentation
import Data.EitherR (fmapL, fmapLT)

{- $conversion
    Use these functions to convert between 'Maybe', 'Either', 'MaybeT', and
    'EitherT'.

    Note that 'hoistEither' is provided by the @either@ package.
-}
-- | Suppress the 'Left' value of an 'Either'
hush :: Either a b -> Maybe b
hush = either (const Nothing) Just

-- | Suppress the 'Left' value of an 'EitherT'
hushT :: (Monad m) => EitherT a m b -> MaybeT m b
hushT = MaybeT . liftM hush . runEitherT

-- | Tag the 'Nothing' value of a 'Maybe'
note :: a -> Maybe b -> Either a b
note a = maybe (Left a) Right

-- | Tag the 'Nothing' value of a 'MaybeT'
noteT :: (Monad m) => a -> MaybeT m b -> EitherT a m b
noteT a = EitherT . liftM (note a) . runMaybeT

-- | Lift a 'Maybe' to the 'MaybeT' monad
hoistMaybe :: (Monad m) => Maybe b -> MaybeT m b
hoistMaybe = MaybeT . return

-- | Case analisys for 'MaybeT'. Evaluates to the first parameter if the
-- 'MaybeT' computation fails, otherwise it applies the given action to the
-- result of the succeding computation.
maybeT :: Monad m => m b -> (a -> m b) -> MaybeT m a -> m b
maybeT mb kb (MaybeT ma) = ma >>= maybe mb kb

-- | Returns whether argument is a 'Left'
isLeft :: Either a b -> Bool
isLeft = either (const True) (const False)

-- | Returns whether argument is a 'Right'
isRight :: Either a b -> Bool
isRight = either (const False) (const True)

-- | 'fmap' specialized to 'Either', given a name symmetric to 'fmapL'
fmapR :: (a -> b) -> Either l a -> Either l b
fmapR = fmap

-- | 'fmap' specialized to 'EitherT', given a name symmetric to 'fmapLT'
fmapRT :: (Functor m) => (a -> b) -> EitherT l m a -> EitherT l m b
fmapRT = fmap

-- | Write a string to standard error
err :: String -> IO ()
err = hPutStr stderr

-- | Write a string with a newline to standard error
errLn :: String -> IO ()
errLn = hPutStrLn stderr

-- | Catch 'IOException's and convert them to the 'EitherT' monad
tryIO :: (MonadIO m) => IO a -> EitherT IOException m a
tryIO = EitherT . liftIO . try
