module Data.Accounts where

import Bank.Error
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as M

-- | Account type
type Account = Int

-- | Balance storage for pure computations
type Accounts = IntMap

-- | Result of operations
type Result = Either BankError

-- (1 балл) Реализуйте вспомогательные чистые функции @nextAccount@,
-- @getBalance@ и @modifyBalance@.
-- Функции из модуля @Data.IntMap.Strict@ можно вызывать с префиксом @M.@:
-- @
--    M.elems . M.delete 1 . M.insert 0 "wow"
-- @
maxInt = 4294967295

newAccount :: Accounts b -> Account
-- ^ Finds fresh account name
newAccount mp = if M.null mp then 0
                             else let x = fst(M.findMax mp) in case x of
                                                            maxInt -> error "NoMoreKeysInMap"
                                                            _ -> x + 1

balance :: Account -> Accounts b -> Result b
-- ^ Gets balance of an account, if it exists
balance acc mp = let x = M.lookup acc mp in case x of
                                          Just a -> Right a
                                          Nothing -> Left CantFindAccount

modifyBalance ::
  -- | Account to modify balance for
  Account ->
  -- | Action to execute on its balance
  (b -> Result b) ->
  -- | Balance storage
  Accounts b ->
  -- | Result of action
  Result (Accounts b)
modifyBalance acc act mp = let x = M.lookup acc mp in case x of
                                          Just val -> let k = act val in case k of
                                                                              Left err -> Left err
                                                                              Right a -> Right $ M.adjust (\x -> a) acc mp
                                          Nothing -> Left CantFindAccount
