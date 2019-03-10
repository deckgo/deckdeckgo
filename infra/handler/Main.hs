import qualified API
import System.IO
import qualified Servant as Servant
import qualified Network.Wai.Handler.Lambda as Lambda

main :: IO ()
main = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  Lambda.run $ Servant.serve API.api server

server :: Servant.Server API.API
server = pure []
