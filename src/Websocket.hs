module Websocket where
    import Data.Text as T
    import qualified Data.ByteString.Lazy as BL
    import qualified Network.WebSockets.Connection   as WS
    import Control.Monad (forever)
    import Data.Typeable

    startSocket pending = do
        conn <- WS.acceptRequest pending
        
        img  <- BL.readFile "img.jpg"
        
        WS.sendBinaryData conn img

        listenForScene conn

    listenForScene conn = forever $ do
        msg <- WS.receiveData conn :: IO BL.ByteString
        putStrLn $ show msg
        print (typeOf msg)
        -- WS.sendTextData conn $ msg
