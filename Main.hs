import System.Environment

main = do
    args <- getArgs
    case args of
        [host] -> putStrLn $ "Connecting to " ++ host ++ "..."
        _ -> putStrLn "Please specify the address of the host computer."

