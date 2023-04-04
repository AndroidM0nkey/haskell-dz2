{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Main where

--local libs
import Bank.Pure

--servant
import Servant
import Data.Aeson.Types
import Servant.API.Generic
import Network.Wai
import Network.Wai.Handler.Warp

-- general libs
import qualified Data.IntMap.Strict as M
import Data.IORef
import Control.Monad.IO.Class
import Control.Monad.State
import Data.Monoid (Sum(..))


-- API type
type UserAPI1 = "new_account" :> Get '[JSON] Acc
           :<|> "balance" :> Capture "acc" Int :> Get '[JSON] Bal
           :<|> "deposit" :> Capture "acc" Int :> Capture "amount" (Sum Int) :> Get '[JSON] Ans
           :<|> "withdraw" :> Capture "acc" Int :> Capture "amount" (Sum Int) :> Get '[JSON] Ans



-- JSON that will be used in answer
data Acc = Acc
  { account_id :: Int
  } deriving (Eq, Show, Generic)

instance ToJSON Acc

data Bal = Bal
  { balance :: Int
  } deriving (Eq, Show, Generic)

instance ToJSON Bal

data Ans = Ans
  { answer :: String
  } deriving (Eq, Show, Generic)

instance ToJSON Ans

server1 ref = processNewAccount ref
         :<|> processBalance ref
         :<|> processDeposit ref
         :<|> processWithdraw ref
        where processNewAccount ref = do
                iostate <- liftIO $ readIORef ref
                let s = evalStateT (runPureBank newAccountHttp) iostate in case s of
                  -- эта ручка всегда возвращает что-то валидное
                  Right s2 -> do
                    let rs = runStateT (runPureBank newAccountHttp) iostate in case rs of
                      Right rs2 -> do
                        tmp <- liftIO $ writeIORef ref (snd rs2)
                        return $ Acc s2

              processBalance ref acc = do
                iostate <- liftIO $ readIORef ref
                let s = evalStateT (runPureBank $ balanceHttp acc) iostate in case s of
                  --TODO: добавить обработку ошибки
                  Right rs -> return $ Bal (getSum rs)

              processDeposit ref acc amount = do
                iostate <- liftIO $ readIORef ref
                let s = runStateT (runPureBank $ depositHttp acc amount) iostate in case s of
                  --TODO: добавить обработку ошибки
                  Right rs -> do
                    tmp <- liftIO $ writeIORef ref (snd rs)
                    return $ Ans "success"

              processWithdraw ref acc amount = do
                iostate <- liftIO $ readIORef ref
                let s = runStateT (runPureBank $ withdrawHttp acc amount) iostate in case s of
                  --TODO: добавить обработку ошибки
                  Right rs -> do
                    tmp <- liftIO $ writeIORef ref (snd rs)
                    return $ Ans "success"



userAPI :: Proxy UserAPI1
userAPI = Proxy

app1 ref = serve userAPI (server1 ref)

main :: IO ()
main = do
    ref <- newIORef M.empty
    run 8081 (app1 ref)
