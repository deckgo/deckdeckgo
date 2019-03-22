import qualified Network.AWS as Aws
import qualified DeckGo.Handler
import qualified Network.Wai.Handler.Warp as Warp

main :: IO ()
main = do
  env <- Aws.newEnv Aws.Discover
  Warp.run 8080 $ DeckGo.Handler.application env
