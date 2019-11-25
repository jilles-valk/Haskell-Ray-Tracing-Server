module Websocket where
    import qualified Data.ByteString.Lazy as BL
    import qualified Network.WebSockets.Connection   as WS
    import Control.Monad (forever)
    import Control.Monad.State
    import Control.Concurrent
    import Data.Typeable
    import Render

    startSocket :: WS.PendingConnection -> IO b
    startSocket pending = do
        conn <- WS.acceptRequest pending
        listenForScene conn

    listenForScene :: WS.Connection -> IO b
    listenForScene conn = forever $ do
        msg <- WS.receiveData conn :: IO BL.ByteString
        sendImage conn (render msg)

    sendImage :: WS.Connection -> BL.ByteString -> IO ()
    sendImage conn img = do 
        WS.sendBinaryData conn img
