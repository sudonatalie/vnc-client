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

    -- Allow shared desktop
    send sock (intsToBytestring [1])

    -- Get ServerInit message
    msg <- recv sock 20
    let serverInit = bytestringToInts msg
    let framebufferWidth = 256 * serverInit !! 0 + serverInit !! 1
    let framebufferHeight = 256 * serverInit !! 2 + serverInit !! 3
    let bitsPerPixel = serverInit !! 4
    let depth = serverInit !! 5
    let bigEndianFlag = serverInit !! 6
    let trueColourFlag = serverInit !! 7
    let redMax = 256 * serverInit !! 8 + serverInit !! 9
    let blueMax = 256 * serverInit !! 10 + serverInit !! 11
    let greenMax = 256 * serverInit !! 12 + serverInit !! 13
    let redShift = serverInit !! 14
    let greenShift = serverInit !! 15
    let blueShift = serverInit !! 16
    -- Last 3 bytes for padding
    putStrLn $ "serverInit: " ++ show serverInit
    putStrLn $ "framebufferWidth: " ++ show framebufferWidth
    putStrLn $ "framebufferHeight: " ++ show framebufferHeight
    putStrLn $ "bitsPerPixel: " ++ show bitsPerPixel
    putStrLn $ "depth: " ++ show depth
    putStrLn $ "bigEndianFlag: " ++ show bigEndianFlag
    putStrLn $ "trueColourFlag: " ++ show trueColourFlag
    putStrLn $ "redMax: " ++ show redMax
    putStrLn $ "blueMax: " ++ show blueMax
    putStrLn $ "greenMax: " ++ show greenMax
    putStrLn $ "redShift: " ++ show redShift
    putStrLn $ "greenShift: " ++ show greenShift
    putStrLn $ "blueShift: " ++ show blueShift

    -- Close socket
    sClose sock

bytestringToInts :: B8.ByteString -> [Int]
bytestringToInts b = map ord (B8.unpack b)

intsToBytestring :: [Int] -> B8.ByteString
intsToBytestring b = B8.pack (map chr b)

