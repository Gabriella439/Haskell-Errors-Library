{-|
    Use this module to convert between 'Maybe', 'Either', 'MaybeT', and
    'EitherT'.
-}

module Control.Error.Util where

import Control.Monad
import Control.Monad.Trans.Either
import Control.Monad.Trans.Maybe

-- | Suppress the 'Left' value of an 'Either'
hush :: Either a b -> Maybe b
hush e = case e of
    Left  _ -> Nothing
    Right b -> Just b

-- | Suppress the 'Left' value of an 'EitherT'
hushT :: (Monad m) => EitherT a m b -> MaybeT m b
hushT = MaybeT . liftM hush . runEitherT

-- | Tag the 'Nothing' value of a 'Maybe'
note :: a -> Maybe b -> Either a b
note a m = case m of
    Nothing -> Left  a
    Just b  -> Right b

-- | Tag the 'Nothing' value of a 'MaybeT'
noteT :: (Monad m) => a -> MaybeT m b -> EitherT a m b
noteT a = EitherT . liftM (note a) . runMaybeT

-- | Lift a 'Maybe' to the 'MaybeT' monad
liftMaybe :: (Monad m) => Maybe b -> MaybeT m b
liftMaybe = MaybeT . return

-- | Lift an 'Either' to the 'EitherT' monad
liftEither :: (Monad m) => Either a b -> EitherT a m b
liftEither = EitherT . return
