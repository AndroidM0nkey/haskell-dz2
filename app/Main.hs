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

-- stm
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM


-- API type
type UserAPI1 = "new_account" :> Get '[JSON] Acc
           :<|> "balance" :> Capture "acc" Int :> Get '[JSON] Bal
           :<|> "deposit" :> Capture "acc" Int :> Capture "amount" (Sum Int) :> Get '[JSON] Ans
           :<|> "withdraw" :> Capture "acc" Int :> Capture "amount" (Sum Int) :> Get '[JSON] Ans
           :<|> "delete" :> Capture "acc" Int :> Get '[JSON] Ans
           :<|> "transfer" :> Capture "from" Int :> Capture "amount" (Sum Int) :> Capture "to" Int :> Get '[JSON] Ans


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
         :<|> processDelete ref
         :<|> processTransfer ref
        where processNewAccount ref = liftIO $ atomically $ do
                iostate <- readTVar ref
                let s = evalStateT (runPureBank newAccountHttp) iostate in case s of
                  -- эта ручка всегда возвращает что-то валидное
                  Right s2 -> do
                    let rs = runStateT (runPureBank newAccountHttp) iostate in case rs of
                      Right rs2 -> do
                        tmp <- writeTVar ref (snd rs2)
                        return $ Acc s2

              processBalance ref acc = liftIO $ atomically $ do
                iostate <- readTVar ref
                let s = evalStateT (runPureBank $ balanceHttp acc) iostate in case s of
                  --TODO: добавить обработку ошибки
                  Right rs -> return $ Bal (getSum rs)

              processDeposit ref acc amount = liftIO $ atomically $ do
                iostate <- readTVar ref
                let oldState = iostate
                let s = runStateT (runPureBank $ depositHttp acc amount) iostate
                case s of
                    Right rs -> do
                      writeTVar ref (snd rs)
                      let newState = snd rs
                      if oldState == newState
                        then return $ Ans "fail"
                        else return $ Ans "success"

              processWithdraw ref acc amount = liftIO $ atomically $ do
                iostate <- readTVar ref
                let oldState = iostate
                let s = runStateT (runPureBank $ withdrawHttp acc amount) iostate
                case s of
                    Right rs -> do
                      writeTVar ref (snd rs)
                      let newState = snd rs
                      if oldState == newState
                        then return $ Ans "fail"
                        else return $ Ans "success"

              -- processWithdraw ref acc amount = do
              --   iostate <- readTVar ref
              --   let s = runStateT (runPureBank $ withdrawHttp acc amount) iostate in case s of
              --     --TODO: добавить обработку ошибки
              --     Right rs -> do
              --       tmp <- writeTVar ref (snd rs)
              --       return $ Ans "success"

              processDelete ref acc = liftIO $ atomically $ do
                iostate <- readTVar ref
                let oldState = iostate
                let s = runStateT (runPureBank $ deleteAccountHttp acc) iostate in case s of
                  Right rs -> do
                    writeTVar ref (snd rs)
                    let newState = snd rs
                    if oldState == newState
                      then return $ Ans "fail"
                      else return $ Ans "success"

              processTransfer ref from amount to = liftIO $ atomically $ do
                iostate <- readTVar ref
                let oldState = iostate
                let s = runStateT (runPureBank $ transferHttp from amount to) iostate in case s of
                  --TODO: добавить обработку ошибки
                  Right rs -> do
                    writeTVar ref (snd rs)
                    let newState = snd rs
                    if oldState == newState
                      then return $ Ans "fail"
                      else return $ Ans "success"



userAPI :: Proxy UserAPI1
userAPI = Proxy

app1 ref = serve userAPI (server1 ref)

main :: IO ()
main = do
    ref <- atomically $ newTVar M.empty
    run 8081 (app1 ref)
