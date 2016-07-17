{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.SWAPI.Swapi where

  import Network.SWAPI.Types
  import Servant.API
  import Servant.Client
  import Control.Monad.Trans.Except (ExceptT)
  import Network.HTTP.Client (Manager)
  import Data.Proxy

  type API = "people" :> Get '[JSON] [Person]
        :<|> "people" :> Capture "id" Int :> Get '[JSON] Person

  getPeople :: Manager -> BaseUrl -> ExceptT ServantError IO [Person]
  getPerson :: Int -> Manager -> BaseUrl -> ExceptT ServantError IO Person


  swapi :: Proxy API
  swapi = Proxy

  getPeople :<|> getPerson = client swapi
