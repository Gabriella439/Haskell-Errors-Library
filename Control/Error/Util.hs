-- | This module exports miscellaneous error-handling functions.

module Control.Error.Util (
    -- * Conversion
    -- $conversion
    hush,
    hushT,
    note,
    noteT,
    hoistMaybe,
    (??),
    (!?),
    failWith,
    failWithM,

    -- * MaybeT
    maybeT,
    just,
    nothing,

    -- * Either
    isLeft,
    isRight,
    fmapR,
    AllE(..),
    AnyE(..),

    -- * EitherT
    fmapRT,

    -- * Error Reporting
    err,
    errLn,

    -- * Exceptions
    tryIO,
    syncIO 
    ) where

import Control.Applicative (Applicative, pure, (<$>))
import qualified Control.Exception as Ex
import Control.Monad (liftM)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Either (EitherT(EitherT, runEitherT))
import Control.Monad.Trans.Maybe (MaybeT(MaybeT, runMaybeT))
import Data.Dynamic (Dynamic)
import Data.Monoid (Monoid(mempty, mappend))
import System.Exit (ExitCode)
import System.IO (hPutStr, hPutStrLn, stderr)

-- For Documentation
import Data.EitherR (fmapL, fmapLT)

{- $conversion
    Use these functions to convert between 'Maybe', 'Either', 'MaybeT', and
    'EitherT'.

    Note that 'hoistEither' and 'eitherT' are provided by the @either@ package.
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

-- | Convert a 'Maybe' value into the 'EitherT' monad
(??) :: Applicative m => Maybe a -> e -> EitherT e m a
(??) a e = EitherT (pure $ note e a)

-- | Convert an applicative 'Maybe' value into the 'EitherT' monad
(!?) :: Applicative m => m (Maybe a) -> e -> EitherT e m a
(!?) a e = EitherT (note e <$> a)

{-| Convert a 'Maybe' value into the 'EitherT' monad

    Named version of `(??)` with arguments flipped
-}
failWith :: Applicative m => e -> Maybe a -> EitherT e m a
failWith e a = a ?? e

{- | Convert an applicative 'Maybe' value into the 'EitherT' monad

    Named version of `(!?)` with arguments flipped
-}
failWithM :: Applicative m => e -> m (Maybe a) -> EitherT e m a
failWithM e a = a !? e

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

{-| Run multiple 'Either' computations and succeed if all of them succeed

    'mappend's all successes or failures
-}
newtype AllE e r = AllE { runAllE :: Either e r }

instance (Monoid e, Monoid r) => Monoid (AllE e r) where
    mempty = AllE (Right mempty)
    mappend (AllE (Right x)) (AllE (Right y)) = AllE (Right (mappend x y))
    mappend (AllE (Right _)) (AllE (Left  y)) = AllE (Left y)
    mappend (AllE (Left  x)) (AllE (Right _)) = AllE (Left x)
    mappend (AllE (Left  x)) (AllE (Left  y)) = AllE (Left  (mappend x y))

{-| Run multiple 'Either' computations and succeed if any of them succeed

    'mappend's all successes or failures
-}
newtype AnyE e r = AnyE { runAnyE :: Either e r }

instance (Monoid e, Monoid r) => Monoid (AnyE e r) where
    mempty = AnyE (Right mempty)
    mappend (AnyE (Right x)) (AnyE (Right y)) = AnyE (Right (mappend x y))
    mappend (AnyE (Right x)) (AnyE (Left  _)) = AnyE (Right x)
    mappend (AnyE (Left  _)) (AnyE (Right y)) = AnyE (Right y)
    mappend (AnyE (Left  x)) (AnyE (Left  y)) = AnyE (Left  (mappend x y))

-- | 'fmap' specialized to 'EitherT', given a name symmetric to 'fmapLT'
fmapRT :: (Monad m) => (a -> b) -> EitherT l m a -> EitherT l m b
fmapRT = liftM

-- | Write a string to standard error
err :: String -> IO ()
err = hPutStr stderr

-- | Write a string with a newline to standard error
errLn :: String -> IO ()
errLn = hPutStrLn stderr

-- | Catch 'Ex.IOException's and convert them to the 'EitherT' monad
tryIO :: (MonadIO m) => IO a -> EitherT Ex.IOException m a
tryIO = EitherT . liftIO . Ex.try

{-| Catch all exceptions, except for asynchronous exceptions found in @base@
    and convert them to the 'EitherT' monad
-}
syncIO :: MonadIO m => IO a -> EitherT Ex.SomeException m a
syncIO a = EitherT . liftIO $ Ex.catches (Right <$> a)
    [ Ex.Handler $ \e -> Ex.throw (e :: Ex.ArithException)
    , Ex.Handler $ \e -> Ex.throw (e :: Ex.ArrayException)
    , Ex.Handler $ \e -> Ex.throw (e :: Ex.AssertionFailed)
    , Ex.Handler $ \e -> Ex.throw (e :: Ex.AsyncException)
    , Ex.Handler $ \e -> Ex.throw (e :: Ex.BlockedIndefinitelyOnMVar)
    , Ex.Handler $ \e -> Ex.throw (e :: Ex.BlockedIndefinitelyOnSTM)
    , Ex.Handler $ \e -> Ex.throw (e :: Ex.Deadlock)
    , Ex.Handler $ \e -> Ex.throw (e ::    Dynamic)
    , Ex.Handler $ \e -> Ex.throw (e :: Ex.ErrorCall)
    , Ex.Handler $ \e -> Ex.throw (e ::    ExitCode)
    , Ex.Handler $ \e -> Ex.throw (e :: Ex.NestedAtomically)
    , Ex.Handler $ \e -> Ex.throw (e :: Ex.NoMethodError)
    , Ex.Handler $ \e -> Ex.throw (e :: Ex.NonTermination)
    , Ex.Handler $ \e -> Ex.throw (e :: Ex.PatternMatchFail)
    , Ex.Handler $ \e -> Ex.throw (e :: Ex.RecConError)
    , Ex.Handler $ \e -> Ex.throw (e :: Ex.RecSelError)
    , Ex.Handler $ \e -> Ex.throw (e :: Ex.RecUpdError)
    , Ex.Handler $ return . Left
    ]
