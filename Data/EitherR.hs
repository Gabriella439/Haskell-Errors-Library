{-|
    This modules provides newtypes which flip the type variables of 'Either'
    and 'EitherT' to access the symmetric monad over the opposite type variable.

    This module provides the following simple benefits to the casual user:

    * A type-class free alternative to @MonadError@

    * No @UndecidableInstances@ or any other extensions, for that matter

    * A more powerful 'catchE' statement that allows you to change the type of
      error value returned

    More advanced users can take advantage of the fact that 'EitherR' and
    'EitherRT' define an entirely symmetric \"success monad\" where
    error-handling computations are the default and successful results terminate
    the monad.  This allows you to chain error-handlers and pass around values
    other than exceptions until you can finally recover from the error:

> runEitherRT $ do
>     e2 <- ioExceptionHandler e1
>     bool <- arithmeticExceptionhandler e2
>     when bool $ lift $ putStrLn "DEBUG: Arithmetic handler did something"

    If any of the above error handlers 'succeed', no other handlers are tried.
-}

module Data.EitherR (
    EitherR(..),
    -- ** Operations in the EitherR monad
    succeed,
    -- ** Conversions to the Either monad
    throwE,
    catchE,
    handleE,
    fmapL,
    -- * EitherRT
    EitherRT(..),
    -- ** Operations in the EitherRT monad
    right,
    succeedT,
    -- ** Conversions to the EitherT monad
    left,
    throwT,
    catchT,
    handleT,
    fmapLT
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either hiding (right)

{-|
    If \"@Either e r@\" is the error monad, then \"@EitherR r e@\" is the
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
    return = EitherR . Left
    (EitherR m) >>= f = EitherR $ case m of
        Left  e -> runEitherR (f e)
        Right r -> Right r

-- | Complete error handling, returning a result
succeed :: r -> EitherR r e
succeed = EitherR . return

-- | 'throwE' in the error monad corresponds to 'return' in the success monad
throwE :: e -> Either e r
throwE = runEitherR . return

-- | 'catchE' in the error monad corresponds to ('>>=') in the success monad
catchE :: Either a r -> (a -> Either b r) -> Either b r
e `catchE` f = runEitherR $ (EitherR e) >>= (EitherR . f)

-- | 'catchE' with the arguments flipped
handleE :: (a -> Either b r) -> Either a r -> Either b r
handleE = flip catchE

-- | Map a function over the 'Left' value of an 'Either'
fmapL :: (a -> b) -> Either a r -> Either b r
fmapL f = runEitherR . fmap f . EitherR

-- | 'EitherR' converted into a monad transformer
newtype EitherRT r m e = EitherRT { runEitherRT :: EitherT e m r }

instance (Monad m) => Functor (EitherRT r m) where
    fmap = liftM

instance (Monad m) => Applicative (EitherRT r m) where
    pure  = return
    (<*>) = ap

instance (Monad m) => Monad (EitherRT r m) where
    return = EitherRT . left
    m >>= f = EitherRT $ EitherT $ do
        x <- runEitherT $ runEitherRT m
        runEitherT $ runEitherRT $ case x of
            Left  e -> f e
            Right r -> right r

instance MonadTrans (EitherRT r) where
    lift = EitherRT . EitherT . liftM Left

-- | The dual to 'left' and synonymous with 'succeedT'
right :: (Monad m) => r -> EitherRT r m e
right = EitherRT . return

-- | Complete error handling, returning a result
succeedT :: (Monad m) => r -> EitherRT r m e
succeedT = right

-- | 'throwT' in the error monad corresponds to 'return' in the success monad
throwT :: (Monad m) => e -> EitherT e m r
throwT = runEitherRT . return

-- | 'catchT' in the error monad corresponds to ('>>=') in the success monad
catchT :: (Monad m) => EitherT a m r -> (a -> EitherT b m r) -> EitherT b m r
e `catchT` f = runEitherRT $ (EitherRT e) >>= (EitherRT . f)

-- | 'catchT' with the arguments flipped
handleT :: (Monad m) => (a -> EitherT b m r) -> EitherT a m r -> EitherT b m r
handleT = flip catchT

-- | Map a function over the 'Left' value of an 'EitherT'
fmapLT :: (Monad m) => (a -> b) -> EitherT a m r -> EitherT b m r
fmapLT f = runEitherRT . fmap f . EitherRT
