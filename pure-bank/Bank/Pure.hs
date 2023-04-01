{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Bank.Pure where

import Bank (MonadBank(..), transfer)
import Bank.Error
import Control.Monad.State
import Control.Monad.Trans.Except
import Data.Accounts (Account, Accounts, modifyBalance, newAccount, balance)
import Data.Monoid (Sum(..))
import qualified Data.IntMap.Strict as M


-- (0.5 балла) исправьте тип поля @runPureBank@, чтобы для @PureBank@ можно было
-- выписать инстанс @MonadBank@, для которого выполнены сформулированные Вами
-- законы. Рекомендуется использовать трансформеры монад.

-- | Monad for pure bank computations. @b@ is a balance type,
-- @a@ is a computation result type.

type PureBankT b a = ExceptT BankError (State (Accounts (Sum b))) a

newtype PureBank b a = PureBank {runPureBank :: PureBankT b a}


-- (0.5 балла) сделайте @PureBank b@ монадой. Если в предыдущем задании Вы
-- использовали трансформеры, рекомендуется воспользоваться командой
-- @deriving newtype@.

instance Functor (PureBank b) where
  fmap f (PureBank bank) = PureBank $ fmap f bank

instance Applicative (PureBank b) where
  pure x = PureBank $ pure x
  (PureBank f) <*> (PureBank x) = PureBank $ f <*> x

instance Monad (PureBank b) where
  (PureBank m) >>= k = PureBank $ m >>= runPureBank . k

-- (1 балл) сделайте @PureBank b@ представителем класса @MonadBank@. Заголовок
-- инстанса менять запрещается. Рекомендуется использовать вспомогательные
-- чистые функции, реализованные ранее.

instance (Num b, Ord b) => MonadBank Account (Sum b) (PureBank b) where

  newAccount = PureBank $ do
    state <- get
    let acc = Data.Accounts.newAccount state
    put $ M.insert acc 100 state
    return acc

  balance acc = PureBank $ do
    state <- get
    case Data.Accounts.balance acc state of
      Left err -> throwE err
      Right b -> return b

  deposit acc amount = PureBank $ do
    state <- get
    case Data.Accounts.modifyBalance acc (\x -> Right $ x <> amount) state of
      Left err -> throwE err
      Right s -> put s

  withdraw acc amount = PureBank $ do
    state <- get
    case Data.Accounts.modifyBalance acc (\x -> if x >= amount then Right $ x - amount else Left NotEnoughMoney) state of
      Left err -> throwE err
      Right s -> put s

  deleteAccount acc = PureBank $ do
    state <- get
    let newState = M.delete acc state
    if state == newState
      then throwE CantFindAccount
      else put newState

