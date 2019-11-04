import Server
import Network.Wai.Handler.Warp (run)


main :: IO ()
main = do
    putStrLn $ "http://localhost:8000/"
    run 8000 app