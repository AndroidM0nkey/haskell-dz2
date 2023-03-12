{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bank where

-- | This typeclass contains operations which one should be able to perform on a
-- certain bank system. In @MonadBank a b m@, @a@ is an account type, @b@ is a
-- balance type and @m@ is a monad where all actions are performed.
--
-- If there is no account @x@, operations @balance x@, @deposit x m@ and
-- @withdraw x m@ should all error out in some way.
class (Monad m, Monoid b, Eq b, Ord b) => MonadBank a b m | m -> a, m -> b where
  -- | Creates a new account.
  newAccount :: m a

  -- | @balance x@ returns the balance of account @x@.
  balance :: a -> m b

  -- | @deposit x m@ adds @m@ to the balance of account @x@.
  deposit :: a -> b -> m ()

  -- | @withdraw x m@ subtracts @m@ from the balance of account @x@.
  withdraw :: a -> b -> m ()

  -- | @deleteAccount x@ deletes an account @x@.
  deleteAccount :: a -> m ()

-- (1.5 балла) Допишите ниже законы, которые, по Вашему мнению, должны
-- выполняться для разумной реализации класса @MonadBank@. Для формулировки
-- некоторых из них можно использовать ограничения @Monoid b@, @Ord b@, @Eq b@.

-- ^ @MonadBank@ laws:
--
-- [Линейность депозита]

  law_dep_linear :: b -> b -> m Bool
  law_dep_linear x y = do
    a_1 <- newAccount
    a_2 <- newAccount

    res_1 <- deposit a_1 x -- <- чтобы увидеть ошибку, если она есть
    res_2 <- deposit a_1 y
    b_1 <- balance a_1

    res_3 <- deposit a_2 (x <> y)
    b_2 <- balance a_2

    return $ b_1 == b_2

-- [Невозможность снять больше, чем на счете]

  law_limit_withdraw :: a -> m Bool
  law_limit_withdraw acc = do
    new_acc <- newAccount -- возьмем баланс нового аккаунта, равный не 0 по условию
    b_not_null <- balance new_acc

    b_start <- balance acc

    withdraw acc (b_start <> b_not_null) -- пытаемся снять баланс + (не 0), игнорим ошибку
    b_end <- balance acc

    return $ b_start == b_end

-- [Трансфер не меняет сумму(<>) балансов]

  law_transfer_balance_sum :: a -> a -> b -> m Bool
  law_transfer_balance_sum acc_1 acc_2 tr = do
    b_1 <- balance acc_1
    b_2 <- balance acc_2

    transfer acc_1 tr acc_2

    b_1_end <- balance acc_1
    b_2_end <- balance acc_2

    return $ (b_1 <> b_2) == (b_1_end <> b_2_end)


-- (0.5 балла) Найдите ошибку в реализации `transfer` и исправьте её.

transfer :: MonadBank a b m => a -> b -> a -> m ()
transfer from amount to = withdraw from amount >>= (\x -> deposit to amount)
