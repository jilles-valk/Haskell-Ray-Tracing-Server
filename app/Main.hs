import Server
import Network.Wai.Handler.Warp (run)
import Control.Concurrent
import qualified Network.WebSockets   as WS
import Websocket

main :: IO ()
main = do
    putStrLn "Starting web socket at ws://localhost:9000/"
    forkIO (WS.runServer "10.10.2.136" 9000 $ startSocket)
    putStrLn "Starting web server at http://localhost:8000/"
    run 8000 app