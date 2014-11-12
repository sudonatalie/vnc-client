module RFB.Client where

import Network.Socket hiding (send, recv)
import Network.Socket.ByteString (send, recv)
import qualified Data.ByteString.Char8 as B8

connect :: String -> Int -> IO()
connect host port = withSocketsDo $ do

    -- Connect to server via socket
    addrInfo <- getAddrInfo Nothing (Just host) (Just $ show port)
    let serverAddr = head addrInfo
    sock <- socket (addrFamily serverAddr) Stream defaultProtocol
    Network.Socket.connect sock (addrAddress serverAddr)

    -- Check for VNC server
    send sock $ B8.pack "Anyone there?"
    version <- recv sock 12
    putStrLn $ "Hi, I'm a VNC server running " ++ B8.unpack version

    send sock $ B8.pack "test"
    temp <- recv sock 1
    putStrLn $ "Hi, I'm a VNC server running " ++ B8.unpack temp
    
    temp <- recv sock 4
    putStrLn $ "Hi, I'm a VNC server running " ++ B8.unpack temp

    -- Close socket
    sClose sock

