import System.Environment (getArgs)
import RFB.Client

main = do
    args <- getArgs
    case args of
        [host] -> do
            putStrLn $ "Connecting to " ++ host ++ "..."
            connect host 5900
        _ -> putStrLn "Please specify the address of the host computer."

