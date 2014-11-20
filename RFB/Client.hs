module RFB.Client where

import Network.Socket hiding (send, recv)
import Network.Socket.ByteString (send, recv)
import qualified Data.ByteString.Char8 as B8
import Data.Char (ord, chr)

connect :: String -> Int -> IO()
connect host port = withSocketsDo $ do

    -- Connect to server via socket
    addrInfo <- getAddrInfo Nothing (Just host) (Just $ show port)
    let serverAddr = head addrInfo
    sock <- socket (addrFamily serverAddr) Stream defaultProtocol
    Network.Socket.connect sock (addrAddress serverAddr)

    -- Check for VNC server
    send sock B8.empty
    msg <- recv sock 12
    -- TODO Verify version format
    putStr $ "Server Protocol Version: " ++ B8.unpack msg

    -- TODO Actually compare version numbers before blindy choosing
    let version = "RFB 003.007\n"
    putStr $ "Requsted Protocol Version: " ++ version
    send sock $ B8.pack version

    -- Receive number of security types
    msg <- recv sock 1
    let numberOfSecurityTypes = head (bytestringToInts msg)

    -- Receive security types
    msg <- recv sock numberOfSecurityTypes
    let securityTypes = bytestringToInts msg
    putStrLn $ "Server Security Types: " ++ show securityTypes

    -- TODO Actually check security types before blindy choosing
    send sock (intsToBytestring [1])

    -- I don't know why SecurityResult isn't being sent
    -- msg <- recv sock 1

    -- Close socket
    sClose sock

bytestringToInts :: B8.ByteString -> [Int]
bytestringToInts b = map ord (B8.unpack b)

intsToBytestring :: [Int] -> B8.ByteString
intsToBytestring b = B8.pack (map chr b)

