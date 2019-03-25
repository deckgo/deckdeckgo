import UnliftIO
import qualified Network.AWS as Aws
import qualified DeckGo.Handler
import qualified Network.Wai.Handler.Lambda as Lambda
import qualified Network.Wai.Middleware.Cors as Cors

main :: IO ()
main = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering

  liftIO $ putStrLn "Booting..."
  env <- Aws.newEnv Aws.Discover

  liftIO $ putStrLn "Booted!"
  Lambda.run $ Cors.simpleCors $ DeckGo.Handler.application env
