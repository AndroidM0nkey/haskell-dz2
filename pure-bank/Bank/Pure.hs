{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Bank.Pure where

import Bank (MonadBank(..), transfer)
import Bank.Error
import Control.Monad.State
import Data.Accounts (Account, Accounts, Result, modifyBalance, newAccount, balance)
import Data.Monoid (Sum(..))
import qualified Data.IntMap.Strict as M

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

instance (Num b, Ord b) => MonadBank Account (Sum b) (PureBank b) where

  newAccount = PureBank $ do
    state <- get
    let acc = Data.Accounts.newAccount state
    put $ M.insert acc 100 state
    return acc

  balance acc = PureBank $ do
    state <- get
    let bal = Data.Accounts.balance acc state
    case bal of
      Left err -> return 0
      Right a -> return $ Sum a

  deposit acc (Sum amount) = PureBank $ do
    state <- get
    case Data.Accounts.modifyBalance acc (\x -> Right $ x + amount) state of
      Left err -> return ()
      Right s -> do
        put s
        return ()

  withdraw acc (Sum amount) = PureBank $ do
    state <- get
    case Data.Accounts.modifyBalance acc (\x -> Right $ x - amount) state of
      Left err -> return ()
      Right s -> do
        put s
        return ()

  deleteAccount acc = PureBank $ do
    state <- get
    let s = M.delete acc state
    put s

-- helpers
type HttpBank = PureBank Int

newAccountHttp :: HttpBank (Account)
newAccountHttp =
  Bank.newAccount

balanceHttp :: Account -> HttpBank (Sum Int)
balanceHttp acc = do
  Bank.balance acc

depositHttp :: Account -> (Sum Int) -> HttpBank ()
depositHttp acc amount = do
  Bank.deposit acc amount

withdrawHttp :: Account -> (Sum Int) -> HttpBank ()
withdrawHttp acc amount = do
  Bank.withdraw acc amount

deleteAccountHttp :: Account -> HttpBank ()
deleteAccountHttp acc = do
  Bank.deleteAccount acc

transferHttp :: Account ->  (Sum Int) -> Account -> HttpBank ()
transferHttp from amount to = do
  Bank.transfer from amount to
