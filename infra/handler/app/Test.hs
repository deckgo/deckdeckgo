import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.API
import Servant.Client
import DeckGo.Handler

main :: IO ()
main = do
  manager' <- newManager defaultManagerSettings
  res <- runClientM decksGet' (mkClientEnv manager' (BaseUrl Http "localhost" 8080 ""))
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right books -> putStrLn $ "Got " <> show (length books)

-- 'client' allows you to produce operations to query an API from a client.
decksGet' :: ClientM [WithId DeckId Deck]
((decksGet' :<|> _ :<|> _) :<|> _ ) = client api
