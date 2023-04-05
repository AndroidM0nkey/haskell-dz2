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

newAccount :: Accounts b -> Account
-- ^ Finds fresh account name
newAccount state = if M.null state then 0 else fst(M.findMax state) + 1

balance :: Account -> Accounts b -> Result b
-- ^ Gets balance of an account, if it exists
balance acc state = let x = M.lookup acc state in case x of
                                          Just a -> Right a
                                          Nothing -> Left CantFindAccount

modifyBalance ::
  (Num b, Ord b) =>
  -- | Account to modify balance for
  Account ->
  -- | Action to execute on its balance
  (b -> Result b) ->
  -- | Balance storage
  Accounts b ->
  -- | Result of action
  Result (Accounts b)
modifyBalance acc act state =
  let x = M.lookup acc state
   in case x of
        Just val ->
          let k = act val
           in case k of
                Left err -> Left err
                Right a ->
                  let newBalance = a
                      balanceCheck = newBalance >= 0
                   in if balanceCheck
                        then Right $ M.adjust (\x -> a) acc state
                        else Left CantModifyBalance
        Nothing -> Left CantFindAccount
