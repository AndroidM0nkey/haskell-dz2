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


-- API type
type UserAPI1 = "users" :> Get '[JSON] [User]

-- JSON that will be used in answer
data User = User
  { account_id :: Int
  } deriving (Eq, Show, Generic)

instance ToJSON User

server1 ref = do
    iostate <- liftIO $ readIORef ref
    let s = evalStateT (runPureBank newAccountHttp) iostate in case s of
      -- эта ручка всегда возвращает что-то валидное
      Right s2 -> do
        let rs = runStateT (runPureBank newAccountHttp) iostate in case rs of
          Right rs2 -> do
            tmp <- liftIO $ writeIORef ref (snd rs2)
            return [User s2]

userAPI :: Proxy UserAPI1
userAPI = Proxy

app1 ref = serve userAPI (server1 ref)

main :: IO ()
main = do
    ref <- newIORef M.empty
    run 8081 (app1 ref)
