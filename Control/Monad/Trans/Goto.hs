-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Trans.Goto
-- Copyright   :  (c) Gregory Crosswhite 2010
-- License     :  BSD3
-- Maintainer  :  gcross@phys.washington.edu
-- Stability   :  provisional
-- Portability :  portable
--
-- This module provides a monad and a monad transformer that allow the
-- user to transfer the flow of execution from an arbitrary point of a
-- monadic computation to another monadic computation.
-----------------------------------------------------------------------------


module Control.Monad.Trans.Goto (
    -- * The Goto monad
    Goto,
    runGoto,
    -- * The GotoT monad transformer
    GotoT(..),
    runGotoT,
    -- * Goto operations
    goto
    ) where

import Control.Applicative (Applicative(..),(<*>))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (MonadTrans(..))

import Data.Functor
import Data.Functor.Identity

-- ---------------------------------------------------------------------------
-- | A goto monad, parameterized by the type @r@ of the value to return.
type Goto r = GotoT r Identity

-- | Execute the goto monad computation and return the resulting value.
runGoto :: Goto r r -- ^ the monadic computation to run
        -> r        -- ^ the result of the computation
runGoto = runIdentity . runGotoT

-- ---------------------------------------------------------------------------
-- | A goto monad transformer parameterized by
--
--   * @r@ - the value that will ultimately be returned; and
--
--   * @m@ - the inner monad.
--
-- The 'GotoT' type wraps a monadic value that contains either a pure value or
-- the next place at which the flow of execution should be continued.
newtype GotoT r m a = GotoT { unwrapGotoT :: m (Either (GotoT r m r) a) }

instance Applicative m => Applicative (GotoT r m) where
    pure = GotoT . fmap Right . pure
    (GotoT m) <*> (GotoT x) = GotoT ((fmap h m) <*> x)
      where
        h (Left g) = const (Left g)
        h (Right f) = either Left (Right . f)
instance Functor m => Functor (GotoT r m) where
    fmap f = GotoT . fmap (either Left (Right . f)) . unwrapGotoT
instance Monad m => Monad (GotoT r m) where
    return = GotoT . return . Right
    (GotoT m) >>= f = GotoT $ m >>= either (return . Left) (unwrapGotoT . f)
instance MonadIO m => MonadIO (GotoT r m) where
    liftIO = lift . liftIO
instance MonadTrans (GotoT r) where
    lift = GotoT . (>>= return . Right)

-- | Execute the goto monad computation and return the resulting value.
--
-- 'runGotoT' is implemented by using a ''trampoline'' approach.  It
-- looks at the monadic value and checks whether it is either a 'Left'
-- containing a continuation or a 'Right' containing the result.  As
-- long as it sees a 'Left' it executes the continuation and then
-- feeds the result back into itself.  Thus the computation bounces
-- back to this function (hence the ''trampoline'') as long as the
-- user keeps calling 'goto' until the final result has been obtained.
runGotoT :: Monad m
         => GotoT r m r -- ^ the monadic computation to run
         -> m r         -- ^ the result of the computation
runGotoT (GotoT m) = m >>= either runGotoT return

-- ---------------------------------------------------------------------------
-- | Transfer the flow of executation from an arbitrary point in the current
--   monadic computation to another monadic computation.
--
-- Note that the destination computation must promise to produce the
-- same value that was promised to be returned by the origin
-- computation.  Also, since control is being transferred away from
-- the origin computation, the goto function returns a monadic value that
-- can have an arbitrary type, since the monadic value will never
-- be used by the originating computation.
goto :: Monad m
     => GotoT r m r -- ^ the destination to which to transfer the flow
                    --   of execution
     -> GotoT r m a -- ^ a monadic value that has the effect of
                    --   transferring the flow of execution to the
                    --   specified destination; note that value can
                    --   have an arbitrary type since it will never
                    --   actually be accessed
goto = GotoT . return . Left
