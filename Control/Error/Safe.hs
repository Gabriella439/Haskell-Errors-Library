{-| This module extends the @safe@ library's functions with corresponding
    versions compatible with 'Either' and 'ExceptT', and also provides a few
    'Maybe'-compatible functions missing from @safe@.

    I suffix the 'Either'-compatible functions with @Err@ and prefix the
    'ExceptT'-compatible functions with @try@.

    Note that this library re-exports the 'Maybe' compatible functions from
    @safe@ in the "Control.Error" module, so they are not provided here.

    The \'@Z@\'-suffixed functions generalize the 'Maybe' functions to also work
    with anything that implements 'MonadPlus', including:

    * Lists

    * Most parsers

    * 'ExceptT' (if the left value is a 'Monoid')
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

    -- * ExceptT-compatible functions
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

import Control.Error.Util (note, hoistEither)
import Control.Monad (MonadPlus(mzero))
import Control.Monad.Trans.Except (ExceptT)
import qualified Safe as S

-- | An assertion that fails in the 'Maybe' monad
assertMay :: Bool -> Maybe ()
assertMay = assertZ

-- | A 'fromRight' that fails in the 'Maybe' monad
rightMay :: Either e a -> Maybe a
rightMay = rightZ

-- | A 'tail' that fails in the 'Either' monad
tailErr :: e -> [a] -> Either e [a]
tailErr e = note e . S.tailMay

-- | An 'init' that fails in the 'Either' monad
initErr :: e -> [a] -> Either e [a]
initErr e = note e . S.initMay

-- | A 'head' that fails in the 'Either' monad
headErr :: e -> [a] -> Either e a
headErr e = note e . S.headMay

-- | A 'last' that fails in the 'Either' monad
lastErr :: e -> [a] -> Either e a
lastErr e = note e . S.lastMay

-- | A 'minimum' that fails in the 'Either' monad
minimumErr :: (Ord a) => e -> [a] -> Either e a
minimumErr e = note e . S.minimumMay

-- | A 'maximum' that fails in the 'Either' monad
maximumErr :: (Ord a) => e -> [a] -> Either e a
maximumErr e = note e . S.maximumMay

-- | A 'foldr1' that fails in the 'Either' monad
foldr1Err :: e -> (a -> a -> a) -> [a] -> Either e a
foldr1Err e step xs = note e $ S.foldr1May step xs

-- | A 'foldl1' that fails in the 'Either' monad
foldl1Err :: e -> (a -> a -> a) -> [a] -> Either e a
foldl1Err e step xs = note e $ S.foldl1May step xs

-- | A 'foldl1'' that fails in the 'Either' monad
foldl1Err' :: e -> (a -> a -> a) -> [a] -> Either e a
foldl1Err' e step xs = note e $ S.foldl1May' step xs

-- | A ('!!') that fails in the 'Either' monad
atErr :: e -> [a] -> Int -> Either e a
atErr e xs n = note e $ S.atMay xs n

-- | A 'read' that fails in the 'Either' monad
readErr :: (Read a) => e -> String -> Either e a
readErr e = note e . S.readMay

-- | An assertion that fails in the 'Either' monad
assertErr :: e -> Bool -> Either e ()
assertErr e p = if p then Right () else Left e

-- | A 'fromJust' that fails in the 'Either' monad
justErr :: e -> Maybe a -> Either e a
justErr e = maybe (Left e) Right

-- | A 'tail' that fails in the 'ExceptT' monad
tryTail :: (Monad m) => e -> [a] -> ExceptT e m [a]
tryTail e xs = hoistEither $ tailErr e xs

-- | An 'init' that fails in the 'ExceptT' monad
tryInit :: (Monad m) => e -> [a] -> ExceptT e m [a]
tryInit e xs = hoistEither $ initErr e xs

-- | A 'head' that fails in the 'ExceptT' monad
tryHead :: (Monad m) => e -> [a] -> ExceptT e m a
tryHead e xs = hoistEither $ headErr e xs

-- | A 'last' that fails in the 'ExceptT' monad
tryLast :: (Monad m) => e -> [a] -> ExceptT e m a
tryLast e xs = hoistEither $ lastErr e xs

-- | A 'minimum' that fails in the 'ExceptT' monad
tryMinimum :: (Monad m, Ord a) => e -> [a] -> ExceptT e m a
tryMinimum e xs = hoistEither $ maximumErr e xs

-- | A 'maximum' that fails in the 'ExceptT' monad
tryMaximum :: (Monad m, Ord a) => e -> [a] -> ExceptT e m a
tryMaximum e xs = hoistEither $ maximumErr e xs

-- | A 'foldr1' that fails in the 'ExceptT' monad
tryFoldr1 :: (Monad m) => e -> (a -> a -> a) -> [a] -> ExceptT e m a
tryFoldr1 e step xs = hoistEither $ foldr1Err e step xs

-- | A 'foldl1' that fails in the 'ExceptT' monad
tryFoldl1 :: (Monad m) => e -> (a -> a -> a) -> [a] -> ExceptT e m a
tryFoldl1 e step xs = hoistEither $ foldl1Err e step xs

-- | A 'foldl1'' that fails in the 'ExceptT' monad
tryFoldl1' :: (Monad m) => e -> (a -> a -> a) -> [a] -> ExceptT e m a
tryFoldl1' e step xs = hoistEither $ foldl1Err' e step xs

-- | A ('!!') that fails in the 'ExceptT' monad
tryAt :: (Monad m) => e -> [a] -> Int -> ExceptT e m a
tryAt e xs n = hoistEither $ atErr e xs n

-- | A 'read' that fails in the 'ExceptT' monad
tryRead :: (Monad m, Read a) => e -> String -> ExceptT e m a
tryRead e str = hoistEither $ readErr e str

-- | An assertion that fails in the 'ExceptT' monad
tryAssert :: (Monad m) => e -> Bool -> ExceptT e m ()
tryAssert e p = hoistEither $ assertErr e p

-- | A 'fromJust' that fails in the 'ExceptT' monad
tryJust :: (Monad m) => e -> Maybe a -> ExceptT e m a
tryJust e m = hoistEither $ justErr e m

-- | A 'fromRight' that fails in the 'ExceptT' monad
tryRight :: (Monad m) => Either e a -> ExceptT e m a
tryRight = hoistEither

-- | A 'tail' that fails using 'mzero'
tailZ :: (MonadPlus m) => [a] -> m [a]
tailZ = maybe mzero return . S.tailMay

-- | An 'init' that fails using 'mzero'
initZ :: (MonadPlus m) => [a] -> m [a]
initZ = maybe mzero return . S.initMay

-- | A 'head' that fails using 'mzero'
headZ :: (MonadPlus m) => [a] -> m a
headZ = maybe mzero return . S.headMay

-- | A 'last' that fails using 'mzero'
lastZ :: (MonadPlus m) => [a] -> m a
lastZ = maybe mzero return . S.lastMay

-- | A 'minimum' that fails using 'mzero'
minimumZ :: (MonadPlus m) => (Ord a) => [a] -> m a
minimumZ = maybe mzero return . S.minimumMay

-- | A 'maximum' that fails using 'mzero'
maximumZ :: (MonadPlus m) => (Ord a) => [a] -> m a
maximumZ = maybe mzero return . S.maximumMay

-- | A 'foldr1' that fails using 'mzero'
foldr1Z :: (MonadPlus m) => (a -> a -> a) -> [a] -> m a
foldr1Z step xs = maybe mzero return $ S.foldr1May step xs

-- | A 'foldl1' that fails using 'mzero'
foldl1Z :: (MonadPlus m) => (a -> a -> a) -> [a] -> m a
foldl1Z step xs = maybe mzero return $ S.foldl1May step xs

-- | A 'foldl1'' that fails using 'mzero'
foldl1Z' :: (MonadPlus m) => (a -> a -> a) -> [a] -> m a
foldl1Z' step xs = maybe mzero return $ S.foldl1May' step xs

-- | A ('!!') that fails using 'mzero'
atZ :: (MonadPlus m) => [a] -> Int -> m a
atZ xs n = maybe mzero return $ S.atMay xs n

-- | A 'read' that fails using 'mzero'
readZ :: (MonadPlus m) => (Read a) => String -> m a
readZ = maybe mzero return . S.readMay

-- | An assertion that fails using 'mzero'
assertZ :: (MonadPlus m) => Bool -> m ()
assertZ p = if p then return () else mzero

-- | A 'fromJust' that fails using 'mzero'
justZ :: (MonadPlus m) => Maybe a -> m a
justZ = maybe mzero return

-- | A 'fromRight' that fails using 'mzero'
rightZ :: (MonadPlus m) => Either e a -> m a
rightZ = either (const mzero) return
