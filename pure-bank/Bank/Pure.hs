{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Bank.Pure where

import Bank (MonadBank (..))
import Bank.Error
import Control.Monad.State
import Control.Monad.Trans.Except
import Data.Accounts
import Data.Monoid (Sum)

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as M

-- (0.5 балла) исправьте тип поля @runPureBank@, чтобы для @PureBank@ можно было
-- выписать инстанс @MonadBank@, для которого выполнены сформулированные Вами
-- законы. Рекомендуется использовать трансформеры монад.

-- | Monad for pure bank computations. @b@ is a balance type,
-- @a@ is a computation result type.

type PureBankT b a = ExceptT BankError (State (Accounts b)) a

newtype PureBank b a = PureBank {runPureBank :: Accounts b -> PureBankT b a}

instance Functor (PureBank b) where
  fmap f (PureBank bank) = PureBank $ \accounts -> fmap f (bank accounts)

instance Applicative (PureBank b) where
  pure a = PureBank $ \_ -> pure a
  (PureBank f) <*> (PureBank a) = PureBank $ \accounts -> f accounts <*> a accounts

instance Monad (PureBank b) where
  (PureBank a) >>= f = PureBank $ \accounts -> a accounts >>= \result -> runPureBank (f result) accounts
