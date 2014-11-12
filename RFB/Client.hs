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

    -- Send back agreed version. (Same as server version, for now)
    -- // same error with sending String :: test
    send sock version

    -- Receive number of security type, which is 1 byte. 
    numberOfSecurityTypes <- recv sock 1
    putStrLn $ "The number of security types is: " ++ B8.unpack numberOfSecurityTypes
    
    -- Change the value type to int
    -- // cannot pass compilation
    -- numberInt <- read $ B8.unpack numberOfSecurityTypes
    -- securityTypes <- recv sock numberInt
    -- putStrLn $ "Here are the security types: " ++ B8.unpack securityTypes

    -- Close socket
    sClose sock

