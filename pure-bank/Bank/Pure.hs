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


-- (0.5 балла) исправьте тип поля @runPureBank@, чтобы для @PureBank@ можно было
-- выписать инстанс @MonadBank@, для которого выполнены сформулированные Вами
-- законы. Рекомендуется использовать трансформеры монад.

-- | Monad for pure bank computations. @b@ is a balance type,
-- @a@ is a computation result type.

newtype PureBank b a = PureBank {runPureBank :: StateT (Accounts b) Result a}

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


--testAcc = do
--  let w = runStateT (runPureBank getNewAcc) M.empty in case w of
--    Left err -> print "lox"
--    Right w2 -> let w3 = runStateT (runPureBank (deposit (fst w2) 200)) (snd w2) in case w3 of
--      Left err -> print "lox"
--      Right w4 -> let w5 = evalStateT (runPureBank (getBal (fst w2))) (snd w4) in case w5 of
--        Left err -> print "lox"
--        Right w6 -> print $ getSum w6



