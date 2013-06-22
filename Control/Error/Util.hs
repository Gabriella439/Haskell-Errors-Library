-- | This module exports miscellaneous error-handling functions.

module Control.Error.Util (
    -- * Conversion
    -- $conversion
    hush,
    hushT,
    note,
    noteT,
    hoistMaybe,
    (?),
    (!?),
    -- * MaybeT
    maybeT,
    just,
    nothing,
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
    tryIO,
    attempt
    ) where

import Control.Applicative
import Control.Exception
import Control.Monad (liftM)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Either (EitherT(EitherT, runEitherT))
import Control.Monad.Trans.Maybe (MaybeT(MaybeT, runMaybeT))
import Data.Dynamic (Dynamic)
import System.Exit (ExitCode)
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

-- | Convert a 'Maybe' value into the 'EitherT' monad.
(?) :: Applicative m => Maybe a -> e -> EitherT e m a
(?) a e = EitherT (pure $ note e a)

-- | Convert an applicative 'Maybe' value into the 'EitherT' monad.
(!?) :: Applicative m => m (Maybe a) -> e -> EitherT e m a
(!?) a e = EitherT (note e <$> a)

{-| Case analysis for 'MaybeT'

    Use the first argument if the 'MaybeT' computation fails, otherwise apply
    the function to the successful result.
-}
maybeT :: Monad m => m b -> (a -> m b) -> MaybeT m a -> m b
maybeT mb kb (MaybeT ma) = ma >>= maybe mb kb

-- | Analogous to 'Just' and equivalent to 'return'
just :: (Monad m) => a -> MaybeT m a
just a = MaybeT (return (Just a))

-- | Analogous to 'Nothing' and equivalent to 'mzero'
nothing :: (Monad m) => MaybeT m a
nothing = MaybeT (return Nothing)

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
fmapRT :: (Monad m) => (a -> b) -> EitherT l m a -> EitherT l m b
fmapRT = liftM

-- | Write a string to standard error
err :: String -> IO ()
err = hPutStr stderr

-- | Write a string with a newline to standard error
errLn :: String -> IO ()
errLn = hPutStrLn stderr

-- | Catch 'IOException's and convert them to the 'EitherT' monad
tryIO :: (MonadIO m) => IO a -> EitherT IOException m a
tryIO = EitherT . liftIO . try

-- | Execute the given IO action, catch all exceptions except:
--
--     * 'AsyncException'
--
--     * 'ArithException'
--
--     * 'ErrorCall'
--
--     * 'Dynamic'
--
--     * 'ExitCode'
--
--     * 'ArrayException'
--
--     * 'AsyncException'
--
--     * 'AssertionFailed'
--
--     * 'Deadlock'
--
--     * 'BlockedIndefinitelyOnSTM'
--
--     * 'BlockedIndefinitelyOnMVar'
--
--     * 'NestedAtomically'
--
--     * 'NonTermination'
--
--     * 'NoMethodError'
--
--     * 'RecUpdError'
--
--     * 'RecConError'
--
--     * 'RecSelError'
--
--     * 'PatternMatchFail'
--
-- and convert them to the 'EitherT' monad.
attempt :: MonadIO m => IO a -> EitherT SomeException m a
attempt a = EitherT . liftIO $ catches (Right <$> a)
    [ Handler $ \e -> throw (e :: ArithException)
    , Handler $ \e -> throw (e :: ArrayException)
    , Handler $ \e -> throw (e :: AssertionFailed)
    , Handler $ \e -> throw (e :: AsyncException)
    , Handler $ \e -> throw (e :: BlockedIndefinitelyOnMVar)
    , Handler $ \e -> throw (e :: BlockedIndefinitelyOnSTM)
    , Handler $ \e -> throw (e :: Deadlock)
    , Handler $ \e -> throw (e :: Dynamic)
    , Handler $ \e -> throw (e :: ErrorCall)
    , Handler $ \e -> throw (e :: ExitCode)
    , Handler $ \e -> throw (e :: NestedAtomically)
    , Handler $ \e -> throw (e :: NoMethodError)
    , Handler $ \e -> throw (e :: NonTermination)
    , Handler $ \e -> throw (e :: PatternMatchFail)
    , Handler $ \e -> throw (e :: RecConError)
    , Handler $ \e -> throw (e :: RecSelError)
    , Handler $ \e -> throw (e :: RecUpdError)
    , Handler $ return . Left
    ]
