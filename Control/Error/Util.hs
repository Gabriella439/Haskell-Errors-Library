-- | This module exports miscellaneous error-handling functions.

module Control.Error.Util (
    -- * Conversion
    -- $conversion
    hush,
    hushT,
    note,
    noteT,
    hoistMaybe,
    hoistEither,
    (??),
    (!?),
    failWith,
    failWithM,

    -- * Bool
    bool,

    -- * Maybe
    (?:),

    -- * MaybeT
    maybeT,
    just,
    nothing,
    isJustT,
    isNothingT,

    -- * Either
    isLeft,
    isRight,
    fmapR,
    AllE(..),
    AnyE(..),

    -- * ExceptT
    isLeftT,
    isRightT,
    fmapRT,
    exceptT,
    bimapExceptT,

    -- * Error Reporting
    err,
    errLn,

    -- * Exceptions
    tryIO,
    syncIO
    ) where

import Control.Applicative (Applicative, pure, (<$>))
import Control.Exception (Handler(..), IOException, SomeException)
import Control.Monad (liftM)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Except (ExceptT(ExceptT), runExceptT)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT), runMaybeT)
import Data.Dynamic (Dynamic)
import Data.Monoid (Monoid(mempty, mappend))
import Data.Maybe (fromMaybe)
import System.Exit (ExitCode)
import System.IO (hPutStr, hPutStrLn, stderr)
import UnexceptionalIO (UIO, Unexceptional)

import qualified Control.Exception as Exception
import qualified UnexceptionalIO   as UIO

-- | Fold an 'ExceptT' by providing one continuation for each constructor
exceptT :: Monad m => (a -> m c) -> (b -> m c) -> ExceptT a m b -> m c
exceptT f g (ExceptT m) = m >>= \z -> case z of
    Left  a -> f a
    Right b -> g b
{-# INLINEABLE exceptT #-}

-- | Transform the left and right value
bimapExceptT :: Functor m => (e -> f) -> (a -> b) -> ExceptT e m a -> ExceptT f m b
bimapExceptT f g (ExceptT m) = ExceptT (fmap h m)
  where
    h (Left e)  = Left  (f e)
    h (Right a) = Right (g a)
{-# INLINEABLE bimapExceptT #-}

-- | Upgrade an 'Either' to an 'ExceptT'
hoistEither :: Monad m => Either e a -> ExceptT e m a
hoistEither = ExceptT . return
{-# INLINEABLE hoistEither #-}

{- $conversion
    Use these functions to convert between 'Maybe', 'Either', 'MaybeT', and
    'ExceptT'.
-}
-- | Suppress the 'Left' value of an 'Either'
hush :: Either a b -> Maybe b
hush = either (const Nothing) Just

-- | Suppress the 'Left' value of an 'ExceptT'
hushT :: (Monad m) => ExceptT a m b -> MaybeT m b
hushT = MaybeT . liftM hush . runExceptT

-- | Tag the 'Nothing' value of a 'Maybe'
note :: a -> Maybe b -> Either a b
note a = maybe (Left a) Right

-- | Tag the 'Nothing' value of a 'MaybeT'
noteT :: (Monad m) => a -> MaybeT m b -> ExceptT a m b
noteT a = ExceptT . liftM (note a) . runMaybeT

-- | Lift a 'Maybe' to the 'MaybeT' monad
hoistMaybe :: (Monad m) => Maybe b -> MaybeT m b
hoistMaybe = MaybeT . return

-- | Convert a 'Maybe' value into the 'ExceptT' monad
(??) :: Applicative m => Maybe a -> e -> ExceptT e m a
(??) a e = ExceptT (pure $ note e a)

-- | Convert an applicative 'Maybe' value into the 'ExceptT' monad
(!?) :: Applicative m => m (Maybe a) -> e -> ExceptT e m a
(!?) a e = ExceptT (note e <$> a)

-- | An infix form of 'fromMaybe' with arguments flipped.
(?:) :: Maybe a -> a -> a
maybeA ?: b = fromMaybe b maybeA
{-# INLINABLE (?:) #-}

{-| Convert a 'Maybe' value into the 'ExceptT' monad

    Named version of ('??') with arguments flipped
-}
failWith :: Applicative m => e -> Maybe a -> ExceptT e m a
failWith e a = a ?? e

{- | Convert an applicative 'Maybe' value into the 'ExceptT' monad

    Named version of ('!?') with arguments flipped
-}
failWithM :: Applicative m => e -> m (Maybe a) -> ExceptT e m a
failWithM e a = a !? e

{- | Case analysis for the 'Bool' type.

   > bool a b c == if c then b else a
-}
bool :: a -> a -> Bool -> a
bool a b = \c -> if c then b else a
{-# INLINABLE bool #-}

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

-- | Analogous to 'Data.Maybe.isJust', but for 'MaybeT'
isJustT :: (Monad m) => MaybeT m a -> m Bool
isJustT = maybeT (return False) (\_ -> return True)
{-# INLINABLE isJustT #-}

-- | Analogous to 'Data.Maybe.isNothing', but for 'MaybeT'
isNothingT :: (Monad m) => MaybeT m a -> m Bool
isNothingT = maybeT (return True) (\_ -> return False)
{-# INLINABLE isNothingT #-}

-- | Returns whether argument is a 'Left'
isLeft :: Either a b -> Bool
isLeft = either (const True) (const False)

-- | Returns whether argument is a 'Right'
isRight :: Either a b -> Bool
isRight = either (const False) (const True)

{- | 'fmap' specialized to 'Either', given a name symmetric to
     'Data.EitherR.fmapL'
-}
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

-- | Analogous to 'isLeft', but for 'ExceptT'
isLeftT :: (Monad m) => ExceptT a m b -> m Bool
isLeftT = exceptT (\_ -> return True) (\_ -> return False)
{-# INLINABLE isLeftT #-}

-- | Analogous to 'isRight', but for 'ExceptT'
isRightT :: (Monad m) => ExceptT a m b -> m Bool
isRightT = exceptT (\_ -> return False) (\_ -> return True)
{-# INLINABLE isRightT #-}

{- | 'fmap' specialized to 'ExceptT', given a name symmetric to
     'Data.EitherR.fmapLT'
-}
fmapRT :: (Monad m) => (a -> b) -> ExceptT l m a -> ExceptT l m b
fmapRT = liftM

-- | Write a string to standard error
err :: String -> IO ()
err = hPutStr stderr

-- | Write a string with a newline to standard error
errLn :: String -> IO ()
errLn = hPutStrLn stderr

-- | Catch 'IOException's and convert them to the 'ExceptT' monad
tryIO :: MonadIO m => IO a -> ExceptT IOException m a
tryIO = ExceptT . liftIO . Exception.try

{-| Catch all exceptions, except for asynchronous exceptions found in @base@
    and convert them to the 'ExceptT' monad
-}
syncIO :: Unexceptional m => IO a -> ExceptT SomeException m a
syncIO = ExceptT . UIO.liftUIO . UIO.fromIO
