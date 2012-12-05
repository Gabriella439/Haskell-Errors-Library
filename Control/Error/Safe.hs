{-|
    This module extends the @safe@ library's functions with corresponding
    versions compatible with 'Either' and 'EitherT', and also provides several
    'Maybe'-compatible functions missing from @safe@.

    All functions take an exceptional value to return should they fail.

    I suffix the 'Either'-compatible functions with @Err@ and prefix the
    'EitherT'-compatible functions with @try@.

    Note that this library re-exports the 'Maybe' compatible functions from
    @safe@ in the "Control.Error" module, so they are not provided here.
-}

module Control.Error.Safe (
    -- * Maybe-compatible functions
    assertMay,
    rightMay,
    -- * Either-compatible functions
    tailErr,
    initErr,
    headErr,
    lastErr,
    minimumErr,
    maximumErr,
    foldr1Err,
    foldl1Err,
    foldl1Err',
    atErr,
    readErr,
    assertErr,
    justErr,
    -- * EitherT-compatible functions
    tryTail,
    tryInit,
    tryHead,
    tryLast,
    tryMinimum,
    tryMaximum,
    tryFoldr1,
    tryFoldl1,
    tryFoldl1',
    tryAt,
    tryRead,
    tryAssert,
    tryJust,
    tryRight,
    -- * MonadPlus-compatible functions
    tailZ,
    initZ,
    headZ,
    lastZ,
    minimumZ,
    maximumZ,
    foldr1Z,
    foldl1Z,
    foldl1Z',
    atZ,
    readZ,
    assertZ,
    justZ,
    rightZ
    ) where

import Control.Error.Util (note)
import Control.Monad (MonadPlus(mzero))
import Control.Monad.Trans.Either (EitherT, hoistEither)
import Safe

-- | An assertion that fails in the 'Maybe' monad
assertMay :: Bool -> Maybe ()
assertMay = assertZ

-- | A 'fromRight' that fails in the 'Maybe' monad
rightMay :: Either e a -> Maybe a
rightMay = either (const Nothing) Just

-- | A 'tail' that fails in the 'Either' monad
tailErr :: e -> [a] -> Either e [a]
tailErr e = note e . tailMay

-- | An 'init' that fails in the 'Either' monad
initErr :: e -> [a] -> Either e [a]
initErr e = note e . initMay

-- | A 'head' that fails in the 'Either' monad
headErr :: e -> [a] -> Either e a
headErr e = note e . headMay

-- | A 'last' that fails in the 'Either' monad
lastErr :: e -> [a] -> Either e a
lastErr e = note e . lastMay

-- | A 'minimum' that fails in the 'Either' monad
minimumErr :: (Ord a) => e -> [a] -> Either e a
minimumErr e = note e . minimumMay

-- | A 'maximum' that fails in the 'Either' monad
maximumErr :: (Ord a) => e -> [a] -> Either e a
maximumErr e = note e . maximumMay

-- | A 'foldr1' that fails in the 'Either' monad
foldr1Err :: e -> (a -> a -> a) -> [a] -> Either e a
foldr1Err e step xs = note e $ foldr1May step xs

-- | A 'foldl1' that fails in the 'Either' monad
foldl1Err :: e -> (a -> a -> a) -> [a] -> Either e a
foldl1Err e step xs = note e $ foldl1May step xs

-- | A 'foldl1'' that fails in the 'Either' monad
foldl1Err' :: e -> (a -> a -> a) -> [a] -> Either e a
foldl1Err' e step xs = note e $ foldl1May' step xs

-- | A ('!!') that fails in the 'Either' monad
atErr :: e -> [a] -> Int -> Either e a
atErr e xs n = note e $ atMay xs n

-- | A 'read' that fails in the 'Either' monad
readErr :: (Read a) => e -> String -> Either e a
readErr e = note e . readMay

-- | An assertion that fails in the 'Either' monad
assertErr :: e -> Bool -> Either e ()
assertErr e p = if p then Right () else Left e

-- | A 'fromJust' that fails in the 'Either' monad
justErr :: e -> Maybe a -> Either e a
justErr e = maybe (Left e) Right

-- | A 'tail' that fails in the 'EitherT' monad
tryTail :: (Monad m) => e -> [a] -> EitherT e m [a]
tryTail e xs = hoistEither $ tailErr e xs

-- | An 'init' that fails in the 'EitherT' monad
tryInit :: (Monad m) => e -> [a] -> EitherT e m [a]
tryInit e xs = hoistEither $ initErr e xs

-- | A 'head' that fails in the 'EitherT' monad
tryHead :: (Monad m) => e -> [a] -> EitherT e m a
tryHead e xs = hoistEither $ headErr e xs

-- | A 'last' that fails in the 'EitherT' monad
tryLast :: (Monad m) => e -> [a] -> EitherT e m a
tryLast e xs = hoistEither $ lastErr e xs

-- | A 'minimum' that fails in the 'EitherT' monad
tryMinimum :: (Monad m, Ord a) => e -> [a] -> EitherT e m a
tryMinimum e xs = hoistEither $ maximumErr e xs

-- | A 'maximum' that fails in the 'EitherT' monad
tryMaximum :: (Monad m, Ord a) => e -> [a] -> EitherT e m a
tryMaximum e xs = hoistEither $ maximumErr e xs

-- | A 'foldr1' that fails in the 'EitherT' monad
tryFoldr1 :: (Monad m) => e -> (a -> a -> a) -> [a] -> EitherT e m a
tryFoldr1 e step xs = hoistEither $ foldr1Err e step xs

-- | A 'foldl1' that fails in the 'EitherT' monad
tryFoldl1 :: (Monad m) => e -> (a -> a -> a) -> [a] -> EitherT e m a
tryFoldl1 e step xs = hoistEither $ foldl1Err e step xs

-- | A 'foldl1'' that fails in the 'EitherT' monad
tryFoldl1' :: (Monad m) => e -> (a -> a -> a) -> [a] -> EitherT e m a
tryFoldl1' e step xs = hoistEither $ foldl1Err' e step xs

-- | A ('!!') that fails in the 'EitherT' monad
tryAt :: (Monad m) => e -> [a] -> Int -> EitherT e m a
tryAt e xs n = hoistEither $ atErr e xs n

-- | A 'read' that fails in the 'EitherT' monad
tryRead :: (Monad m, Read a) => e -> String -> EitherT e m a
tryRead e str = hoistEither $ readErr e str

-- | An assertion that fails in the 'EitherT' monad
tryAssert :: (Monad m) => e -> Bool -> EitherT e m ()
tryAssert e p = hoistEither $ assertErr e p

-- | A 'fromJust' that fails in the 'EitherT' monad
tryJust :: (Monad m) => e -> Maybe a -> EitherT e m a
tryJust e m = hoistEither $ justErr e m

-- | A 'fromRight' that fails in the 'EitherT' monad
tryRight :: (Monad m) => Either e a -> EitherT e m a
tryRight = hoistEither

-- | A 'tail' that fails using 'mzero'
tailZ :: (MonadPlus m) => [a] -> m [a]
tailZ = maybe mzero return . tailMay

-- | An 'init' that fails using 'mzero'
initZ :: (MonadPlus m) => [a] -> m [a]
initZ = maybe mzero return . initMay

-- | A 'head' that fails using 'mzero'
headZ :: (MonadPlus m) => [a] -> m a
headZ = maybe mzero return . headMay

-- | A 'last' that fails using 'mzero'
lastZ :: (MonadPlus m) => [a] -> m a
lastZ = maybe mzero return . lastMay

-- | A 'minimum' that fails using 'mzero'
minimumZ :: (MonadPlus m) => (Ord a) => [a] -> m a
minimumZ = maybe mzero return . minimumMay

-- | A 'maximum' that fails using 'mzero'
maximumZ :: (MonadPlus m) => (Ord a) => [a] -> m a
maximumZ = maybe mzero return . maximumMay

-- | A 'foldr1' that fails using 'mzero'
foldr1Z :: (MonadPlus m) => (a -> a -> a) -> [a] -> m a
foldr1Z step xs = maybe mzero return $ foldr1May step xs

-- | A 'foldl1' that fails using 'mzero'
foldl1Z :: (MonadPlus m) => (a -> a -> a) -> [a] -> m a
foldl1Z step xs = maybe mzero return $ foldl1May step xs

-- | A 'foldl1'' that fails using 'mzero'
foldl1Z' :: (MonadPlus m) => (a -> a -> a) -> [a] -> m a
foldl1Z' step xs = maybe mzero return $ foldl1May' step xs

-- | A ('!!') that fails using 'mzero'
atZ :: (MonadPlus m) => [a] -> Int -> m a
atZ xs n = maybe mzero return $ atMay xs n

-- | A 'read' that fails using 'mzero'
readZ :: (MonadPlus m) => (Read a) => String -> m a
readZ = maybe mzero return . readMay

-- | An assertion that fails using 'mzero'
assertZ :: (MonadPlus m) => Bool -> m ()
assertZ p = if p then return () else mzero

-- | A 'fromJust' that fails using 'mzero'
justZ :: (MonadPlus m) => Maybe a -> m a
justZ = maybe mzero return

-- | A 'fromRight' that fails using 'mzero'
rightZ :: (MonadPlus m) => Either e a -> m a
rightZ = either (const mzero) return
