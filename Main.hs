import System.Environment
import RFB.Client

main = do
    args <- getArgs
    case args of
        [host] -> do
            putStrLn $ "Connecting to " ++ host ++ "..."
            RFB.Client.connect host 5900
        _ -> putStrLn "Please specify the address of the host computer."

