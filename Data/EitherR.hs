{-# LANGUAGE CPP #-}

{-| This module provides 'throwEither' and 'catchEither' for 'Either'.  These two
    functions reside here because 'throwEither' and 'catchEither' correspond to 'return'
    and ('>>=') for the flipped 'Either' monad: 'EitherR'.  Additionally, this
    module defines 'handleE' as the flipped version of 'catchE' for 'ExceptT'.

    'throwEither' and 'catchEither' improve upon @MonadError@ because:

    * 'catchEither' is more general than 'catch' and allows you to change the left value's type

    * Both are Haskell98

    More advanced users can use 'EitherR' and 'ExceptRT' to program in an
    entirely symmetric \"success monad\" where exceptional results are the norm
    and successful results terminate the computation.  This allows you to chain
    error-handlers using @do@ notation and pass around exceptional values of
    varying types until you can finally recover from the error:

> runExceptRT $ do
>     e2   <- ioExceptionHandler e1
>     bool <- arithmeticExceptionhandler e2
>     when bool $ lift $ putStrLn "DEBUG: Arithmetic handler did something"

    If any of the above error handlers 'succeed', no other handlers are tried.

    If you choose not to typefully distinguish between the error and sucess
    monad, then use 'flipEither' and 'flipET', which swap the type variables without
    changing the type.
-}

module Data.EitherR (
    -- * EitherR
    EitherR(..),

    -- ** Operations in the EitherR monad
    succeed,

    -- ** Conversions to the Either monad
    throwEither,
    catchEither,
    handleEither,
    fmapL,

    -- ** Flip alternative
    flipEither,

    -- * ExceptRT
    ExceptRT(..),

    -- ** Operations in the ExceptRT monad
    succeedT,

    -- ** Conversions to the ExceptT monad
    handleE,
    fmapLT,

    -- ** Flip alternative
    flipET,
    ) where

import Control.Applicative (Applicative(pure, (<*>)), Alternative(empty, (<|>)))
import Control.Monad (liftM, ap, MonadPlus(mzero, mplus))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Except (ExceptT(ExceptT), runExceptT, throwE, catchE)
import Data.Monoid (Monoid(mempty, mappend))

import qualified Control.Monad.Trans.Except

{-| If \"@Either e r@\" is the error monad, then \"@EitherR r e@\" is the
    corresponding success monad, where:

    * 'return' is 'throwEither'.

    * ('>>=') is 'catchEither'.

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

-- | 'throwEither' in the error monad corresponds to 'return' in the success monad
throwEither :: e -> Either e r
throwEither e = runEitherR (return e)

-- | 'catchEither' in the error monad corresponds to ('>>=') in the success monad
catchEither :: Either a r -> (a -> Either b r) -> Either b r
e `catchEither` f = runEitherR $ EitherR e >>= \a -> EitherR (f a)

-- | 'catchEither' with the arguments flipped
handleEither :: (a -> Either b r) -> Either a r -> Either b r
handleEither = flip catchEither

-- | Map a function over the 'Left' value of an 'Either'
fmapL :: (a -> b) -> Either a r -> Either b r
fmapL f = runEitherR . fmap f . EitherR

-- | Flip the type variables of 'Either'
flipEither :: Either a b -> Either b a
flipEither e = case e of
    Left  a -> Right a
    Right b -> Left  b

-- | 'EitherR' converted into a monad transformer
newtype ExceptRT r m e = ExceptRT { runExceptRT :: ExceptT e m r }

instance (Monad m) => Functor (ExceptRT r m) where
    fmap = liftM

instance (Monad m) => Applicative (ExceptRT r m) where
    pure  = return
    (<*>) = ap

instance (Monad m) => Monad (ExceptRT r m) where
    return e = ExceptRT (throwE e)
    m >>= f = ExceptRT $ ExceptT $ do
        x <- runExceptT $ runExceptRT m
        runExceptT $ runExceptRT $ case x of
            Left  e -> f e
            Right r -> ExceptRT (return r)

instance (Monad m, Monoid r) => Alternative (ExceptRT r m) where
    empty = ExceptRT $ ExceptT $ return $ Right mempty
    e1 <|> e2 = ExceptRT $ ExceptT $ do
        x1 <- runExceptT $ runExceptRT e1
        case x1 of
            Left  l  -> return (Left l)
            Right r1 -> do
                x2 <- runExceptT $ runExceptRT e2
                case x2 of
                    Left  l  -> return (Left l)
                    Right r2 -> return (Right (mappend r1 r2))

instance (Monad m, Monoid r) => MonadPlus (ExceptRT r m) where
    mzero = empty
    mplus = (<|>)

instance MonadTrans (ExceptRT r) where
    lift = ExceptRT . ExceptT . liftM Left

instance (MonadIO m) => MonadIO (ExceptRT r m) where
    liftIO = lift . liftIO

-- | Complete error handling, returning a result
succeedT :: (Monad m) => r -> ExceptRT r m e
succeedT r = ExceptRT (return r)

-- | 'catchT' with the arguments flipped
handleE :: (Monad m) => (a -> ExceptT b m r) -> ExceptT a m r -> ExceptT b m r
handleE = flip catchE

-- | Map a function over the 'Left' value of an 'ExceptT'
#if MIN_VERSION_base(4,8,0)
fmapLT :: Functor m => (a -> b) -> ExceptT a m r -> ExceptT b m r
fmapLT = Control.Monad.Trans.Except.withExceptT
#else
fmapLT :: (Monad m) => (a -> b) -> ExceptT a m r -> ExceptT b m r
fmapLT f = runExceptRT . fmap f . ExceptRT
#endif

-- | Flip the type variables of an 'ExceptT'
flipET :: (Monad m) => ExceptT a m b -> ExceptT b m a
flipET = ExceptT . liftM flipEither . runExceptT
