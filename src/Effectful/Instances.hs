{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- HLINT ignore "Use const" -}

module Effectful.Instances () where

import Control.Monad.Class.MonadST (MonadST (..))
import Control.Monad.Primitive (stToPrim)
import Effectful (Eff, (:>))
import Effectful.Prim (Prim)

import Control.Monad.Catch qualified as Ex
import Control.Monad.Class.MonadThrow qualified as IOC
import Effectful.Exception qualified as Eff


instance (Prim :> es) => MonadST (Eff es) where
    stToIO = stToPrim
    {-# INLINE stToIO #-}


instance IOC.MonadThrow (Eff es) where
    throwIO = Ex.throwM


instance IOC.MonadCatch (Eff es) where
    catch = Ex.catch


instance IOC.MonadMask (Eff es) where
    mask = Eff.mask
    uninterruptibleMask = Eff.uninterruptibleMask
    getMaskingState = Eff.getMaskingState
    interruptible = Eff.interruptible
