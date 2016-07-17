module Main where
  import Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
  import Network.SWAPI.Types
  import Network.SWAPI.Swapi
  import Servant.API
  import Servant.Client
  import Control.Monad.Trans.Except (ExceptT, runExceptT)
  import Data.Proxy

  queries :: Manager -> BaseUrl -> ExceptT ServantError IO Person
  queries m b = getPerson 14 m b

  main :: IO ()
  main = do
    m <- newManager defaultManagerSettings
    r <- runExceptT (queries m (BaseUrl Http "swapi.co" 80 "/api"))
    case r of
      Left err -> putStrLn $ "Error " ++ show err
      Right person -> print person
