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

-- concurrency
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.MVar
import GHC.Conc


-- API type
type UserAPI1 = "new_account" :> Get '[JSON] Acc
           :<|> "balance" :> Capture "acc" Int :> Get '[JSON] Bal
           :<|> "deposit" :> Capture "acc" Int :> Capture "amount" Int :> Get '[JSON] Ans
--           :<|> "withdraw" :> Capture "acc" Int :> Capture "amount" (Sum Int) :> Get '[JSON] Ans
--           :<|> "delete" :> Capture "acc" Int :> Get '[JSON] Ans
--           :<|> "transfer" :> Capture "from" Int :> Capture "amount" (Sum Int) :> Capture "to" Int :> Get '[JSON] Ans


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
        -- :<|> processWithdraw ref
        -- :<|> processDelete ref
        -- :<|> processTransfer ref
        where processNewAccount ref = liftIO $ atomically $ do
                iostate <- unsafeIOToSTM $ takeMVar ref
                let ind = M.size iostate
                newT <- newTVar 100
                unsafeIOToSTM $ putMVar ref (M.insert ind newT iostate)
                return $ Acc ind

              processBalance ref acc = liftIO $ atomically $ do
                iostate <- unsafeIOToSTM $ readMVar ref
                let bal = M.lookup acc iostate in case bal of
                                        Just a -> do
                                          realbal <- readTVar a
                                          return $ Bal realbal
                                        Nothing -> throwSTM err404

              processDeposit ref acc amount = liftIO $ atomically $ do
                iostate <- unsafeIOToSTM $ takeMVar ref
                let bal = M.lookup acc iostate in case bal of
                                        Just a -> do
                                          realbal <- readTVar a
                                          writeTVar a (realbal + amount)
                                          return $ Ans "success"
                                        Nothing -> throwSTM err404



        --      processBalance ref acc = liftIO $ atomically $ do
        --        iostate <- readTVar ref
        --        let s = evalStateT (runPureBank $ balanceHttp acc) iostate in case s of
        --          Right rs -> return $ Bal (getSum rs)
--
        --      processDeposit ref acc amount = liftIO $ atomically $ do
        --        iostate <- readTVar ref
        --        let oldState = iostate
        --        let s = runStateT (runPureBank $ depositHttp acc amount) iostate
        --        case s of
        --            Right rs -> do
        --              writeTVar ref (snd rs)
        --              let newState = snd rs
        --              if oldState == newState
        --                -- then return $ Ans "fail"
        --                then throwSTM err400
        --                else return $ Ans "success"
--
        --      processWithdraw ref acc amount = liftIO $ atomically $ do
        --        iostate <- readTVar ref
        --        let oldState = iostate
        --        let s = runStateT (runPureBank $ withdrawHttp acc amount) iostate
        --        case s of
        --            Right rs -> do
        --              writeTVar ref (snd rs)
        --              let newState = snd rs
        --              if oldState == newState
        --                then throwSTM err400
        --                else return $ Ans "success"
--
        --      processDelete ref acc = liftIO $ atomically $ do
        --        iostate <- readTVar ref
        --        let oldState = iostate
        --        let s = runStateT (runPureBank $ deleteAccountHttp acc) iostate in case s of
        --          Right rs -> do
        --            writeTVar ref (snd rs)
        --            let newState = snd rs
        --            if oldState == newState
        --              then throwSTM err400
        --              else return $ Ans "success"
--
        --      processTransfer ref from amount to = liftIO $ atomically $ do
        --        iostate <- readTVar ref
        --        let oldState = iostate
        --        let s = runStateT (runPureBank $ transferHttp from amount to) iostate in case s of
        --          Right rs -> do
        --            writeTVar ref (snd rs)
        --            let newState = snd rs
        --            if oldState == newState
        --              then throwSTM err400
        --              else return $ Ans "success"



userAPI :: Proxy UserAPI1
userAPI = Proxy

app1 ref = serve userAPI (server1 ref)

main :: IO ()
main = do
    ref <- newMVar M.empty
    run 8081 (app1 ref)
