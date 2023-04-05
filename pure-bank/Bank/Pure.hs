{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Bank.Pure where

import Bank (MonadBank(..), transfer)
import Bank.Error
import Control.Monad.State
import Control.Monad.Trans.Except
import Data.Accounts (Account, Accounts, Result, modifyBalance, newAccount, balance)
import Data.Monoid (Sum(..))
import qualified Data.IntMap.Strict as M

--stm
import Control.Concurrent.STM

-- | Monad for pure bank computations. @b@ is a balance type,
-- @a@ is a computation result type.

newtype PureBank b a = PureBank {runPureBank :: StateT (Accounts b) Result a}
instance Functor (PureBank b) where
  fmap f (PureBank bank) = PureBank $ fmap f bank

instance Applicative (PureBank b) where
  pure x = PureBank $ pure x
  (PureBank f) <*> (PureBank x) = PureBank $ f <*> x

instance Monad (PureBank b) where
  (PureBank m) >>= k = PureBank $ m >>= runPureBank . k

instance (Num b, Ord b) => MonadBank Account (Sum b) (PureBank (TVar (Sum b)))
