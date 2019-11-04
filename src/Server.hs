{-# LANGUAGE OverloadedStrings #-}
module Server where

    import Network.Wai
    import Network.HTTP.Types
    import Network.Wai.Handler.Warp (run)

    app :: Application
    app request respond = respond $ case rawPathInfo request of
        "/"     -> index
        "/index.js" -> indexJS
        "/index.css" -> indexCSS
        _       -> notFound

    indexJS :: Response
    indexJS = responseFile
        status200
        [("Content-Type", "text/html")]
        "./client/index.js"
        Nothing

    indexCSS :: Response
    indexCSS = responseFile
        status200
        [("Content-Type", "text/css")]
        "./client/index.css"
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