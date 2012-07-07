module Control.Error.Util where

import Control.Monad
import Control.Monad.Trans.Either
import Control.Monad.Trans.Maybe

hush :: Either a b -> Maybe b
hush e = case e of
    Left  _ -> Nothing
    Right b -> Just b

hushT :: (Monad m) => EitherT a m b -> MaybeT m b
hushT = MaybeT . liftM hush . runEitherT

-- A 'Maybe' that fails in the 'Either' monad
note :: e -> Maybe r -> Either e r
note a m = case m of
    Nothing -> Left  a
    Just b  -> Right b

-- A 'MaybeT' that fails in the 'EitherT' monad
noteT :: (Monad m) => e -> MaybeT m r -> EitherT e m r
noteT a = EitherT . liftM (note a) . runMaybeT

-- A 'Maybe' that fails in the 'MaybeT' monad
liftMaybe :: (Monad m) => Maybe r -> MaybeT m r
liftMaybe = MaybeT . return

-- An 'Either' that fails in the 'EitherT' monad
liftEither :: (Monad m) => Either a b -> EitherT a m b
liftEither = EitherT . return

fmapL :: (a -> a') -> Either a b -> Either a' b
fmapL f e = case e of
    Left  a -> Left (f a)
    Right b -> Right b

fmapLT :: (Monad m) => (a -> a') -> EitherT a m b -> EitherT a' m b
fmapLT f = EitherT . liftM (fmapL f) . runEitherT
