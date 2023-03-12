module Bank.Error (BankError (..))
    where

import Control.Exception (Exception)

-- (1 балл) Перечислите всё, что может пойти не так в ходе выполнения операций
-- из @MonadBank@.

data BankError
    = NotEnoughMoney
    | CantFindAccount
    | CantCreateAccount
    | CantModifyBalance
    deriving (Show)

instance Exception BankError
