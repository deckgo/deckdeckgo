import qualified API
import qualified Network.HTTP.Client as HTTP
import qualified Servant.Client as Servant

main :: IO ()
main = do
  manager' <- HTTP.newManager HTTP.defaultManagerSettings
  res <- Servant.runClientM getDecks (Servant.mkClientEnv manager'
    (Servant.BaseUrl Servant.Http "localhost" 8081 ""))
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right books -> print $ length books

getDecks :: Servant.ClientM [API.Deck]
getDecks = Servant.client API.api
