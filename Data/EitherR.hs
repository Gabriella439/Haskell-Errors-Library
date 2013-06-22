{-| This module provides 'throwE' and 'catchE' for 'Either'.  These two
    functions reside here because 'throwE' and 'catchE' correspond to 'return'
    and ('>>=') for the flipped 'Either' monad: 'EitherR'.  Similarly, this
    module defines 'throwT' and 'catchT' for 'EitherT', which correspond to the
    'Monad' operations for 'EitherRT'.

    These throw and catch functions improve upon @MonadError@ because:

    * 'catch' is more general and allows you to change the left value's type

    * They are Haskell98

    More advanced users can use 'EitherR' and 'EitherRT' to program in an
    entirely symmetric \"success monad\" where exceptional results are the norm
    and successful results terminate the computation.  This allows you to chain
    error-handlers using @do@ notation and pass around exceptional values of
    varying types until you can finally recover from the error:

> runEitherRT $ do
>     e2   <- ioExceptionHandler e1
>     bool <- arithmeticExceptionhandler e2
>     when bool $ lift $ putStrLn "DEBUG: Arithmetic handler did something"

    If any of the above error handlers 'succeed', no other handlers are tried.

    If you choose not to typefully distinguish between the error and sucess
    monad, then use 'flipE' and 'flipET', which swap the type variables without
    changing the type.
-}

module Data.EitherR (
    -- * EitherR
    EitherR(..),

    -- ** Operations in the EitherR monad
    succeed,

    -- ** Conversions to the Either monad
    throwE,
    catchE,
    handleE,
    fmapL,

    -- ** Flip alternative
    flipE,

    -- * EitherRT
    EitherRT(..),

    -- ** Operations in the EitherRT monad
    succeedT,

    -- ** Conversions to the EitherT monad
    throwT,
    catchT,
    handleT,
    fmapLT,

    -- ** Flip alternative
    flipET,
    ) where

import Control.Applicative (Applicative(pure, (<*>)), Alternative(empty, (<|>)))
import Control.Monad (liftM, ap, MonadPlus(mzero, mplus))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Either (EitherT(EitherT, runEitherT), left, right)
import Data.Monoid (Monoid(mempty, mappend))

{-| If \"@Either e r@\" is the error monad, then \"@EitherR r e@\" is the
    corresponding success monad, where:

    * 'return' is 'throwE'.

    * ('>>=') is 'catchE'.

    * Successful results abort the computation
-}
newtype EitherR r e = EitherR { runEitherR :: Either e r }

instance Functor (EitherR r) where
    fmap = liftM

instance Applicative (EitherR r) where
    pure  = return
    (<*>) = ap

instance Monad (EitherR r) where
    return e = EitherR (Left e)
    EitherR m >>= f = case m of
        Left  e -> f e
        Right r -> EitherR (Right r)

instance (Monoid r) => Alternative (EitherR r) where
    empty = EitherR (Right mempty)
    e1@(EitherR (Left _)) <|> _ = e1
    _ <|> e2@(EitherR (Left _)) = e2
    EitherR (Right r1) <|> EitherR (Right r2)
        = EitherR (Right (mappend r1 r2))

instance (Monoid r) => MonadPlus (EitherR r) where
    mzero = empty
    mplus = (<|>)

-- | Complete error handling, returning a result
succeed :: r -> EitherR r e
succeed r = EitherR (return r)

-- | 'throwE' in the error monad corresponds to 'return' in the success monad
throwE :: e -> Either e r
throwE e = runEitherR (return e)

-- | 'catchE' in the error monad corresponds to ('>>=') in the success monad
catchE :: Either a r -> (a -> Either b r) -> Either b r
e `catchE` f = runEitherR $ EitherR e >>= \a -> EitherR (f a)

-- | 'catchE' with the arguments flipped
handleE :: (a -> Either b r) -> Either a r -> Either b r
handleE = flip catchE

-- | Map a function over the 'Left' value of an 'Either'
fmapL :: (a -> b) -> Either a r -> Either b r
fmapL f = runEitherR . fmap f . EitherR

-- | Flip the type variables of 'Either'
flipE :: Either a b -> Either b a
flipE e = case e of
    Left  a -> Right a
    Right b -> Left  b

-- | 'EitherR' converted into a monad transformer
newtype EitherRT r m e = EitherRT { runEitherRT :: EitherT e m r }

instance (Monad m) => Functor (EitherRT r m) where
    fmap = liftM

instance (Monad m) => Applicative (EitherRT r m) where
    pure  = return
    (<*>) = ap

instance (Monad m) => Monad (EitherRT r m) where
    return e = EitherRT (left e)
    m >>= f = EitherRT $ EitherT $ do
        x <- runEitherT $ runEitherRT m
        runEitherT $ runEitherRT $ case x of
            Left  e -> f e
            Right r -> EitherRT (right r)

instance (Monad m, Monoid r) => Alternative (EitherRT r m) where
    empty = EitherRT $ EitherT $ return $ Right mempty
    e1 <|> e2 = EitherRT $ EitherT $ do
        x1 <- runEitherT $ runEitherRT e1
        case x1 of
            Left  l  -> return (Left l)
            Right r1 -> do
                x2 <- runEitherT $ runEitherRT e2
                case x2 of
                    Left  l  -> return (Left l)
                    Right r2 -> return (Right (mappend r1 r2))

instance (Monad m, Monoid r) => MonadPlus (EitherRT r m) where
    mzero = empty
    mplus = (<|>)

instance MonadTrans (EitherRT r) where
    lift = EitherRT . EitherT . liftM Left

instance (MonadIO m) => MonadIO (EitherRT r m) where
    liftIO = lift . liftIO

-- | Complete error handling, returning a result
succeedT :: (Monad m) => r -> EitherRT r m e
succeedT r = EitherRT (return r)

-- | 'throwT' in the error monad corresponds to 'return' in the success monad
throwT :: (Monad m) => e -> EitherT e m r
throwT e = runEitherRT (return e)

-- | 'catchT' in the error monad corresponds to ('>>=') in the success monad
catchT :: (Monad m) => EitherT a m r -> (a -> EitherT b m r) -> EitherT b m r
e `catchT` f = runEitherRT $ EitherRT e >>= \a -> EitherRT (f a)

-- | 'catchT' with the arguments flipped
handleT :: (Monad m) => (a -> EitherT b m r) -> EitherT a m r -> EitherT b m r
handleT = flip catchT

-- | Map a function over the 'Left' value of an 'EitherT'
fmapLT :: (Monad m) => (a -> b) -> EitherT a m r -> EitherT b m r
fmapLT f = runEitherRT . fmap f . EitherRT

-- | Flip the type variables of an 'EitherT'
flipET :: (Monad m) => EitherT a m b -> EitherT b m a
flipET = EitherT . liftM flipE . runEitherT
