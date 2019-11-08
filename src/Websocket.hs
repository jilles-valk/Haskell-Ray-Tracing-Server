module Websocket where
    import Data.Text as T
    import qualified Data.ByteString.Lazy as BL
    import qualified Network.WebSockets.Connection   as WS
    import Control.Monad (forever)
    import Data.Typeable
    import Render

    startSocket :: WS.PendingConnection -> IO b
    startSocket pending = do
        conn <- WS.acceptRequest pending
        listenForScene conn

    listenForScene :: WS.Connection -> IO b
    listenForScene conn = forever $ do
        msg <- WS.receiveData conn :: IO BL.ByteString
        putStrLn $ show msg
        print $ typeOf msg
        sendImage conn

    sendImage :: WS.Connection -> IO ()
    sendImage conn = do 
        render "1"
        img  <- BL.readFile "img.jpg"
        WS.sendBinaryData conn img
