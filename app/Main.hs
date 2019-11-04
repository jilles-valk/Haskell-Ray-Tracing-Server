{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)

app :: Application
app request respond = respond $ case rawPathInfo request of
    "/"     -> index
    "/index.js" -> indexJS
    _       -> notFound

indexJS :: Response
indexJS = responseFile
    status200
    [("Content-Type", "text/html")]
    "./client/index.js"
    Nothing

notFound :: Response
notFound = responseLBS
    status404
    [("Content-Type", "text/plain")]
    "404 - Not Found"

index :: Response
index = responseFile
    status200
    [("Content-Type", "text/html")]
    "./client/index.html"
    Nothing



main :: IO ()
main = do
    putStrLn $ "http://localhost:8000/"
    run 8000 app