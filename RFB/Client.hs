module RFB.Client where

import Network.Socket hiding (send, recv)
import Network.Socket.ByteString (send, recv)
import qualified Data.ByteString.Char8 as B8
import Data.Char (ord, chr)
import Data.Bits

connect :: String -> Int -> IO()
connect host port = withSocketsDo $ do

    -- Connect to server via socket
    addrInfo <- getAddrInfo Nothing (Just host) (Just $ show port)
    let serverAddr = head addrInfo
    sock <- socket (addrFamily serverAddr) Stream defaultProtocol
    Network.Socket.connect sock (addrAddress serverAddr)

    -- Check for VNC server
    sendInts sock []
    msg <- recvString sock 12
    -- TODO Verify version format
    putStr $ "Server Protocol Version: " ++ msg

    -- TODO Actually compare version numbers before blindy choosing
    let version = "RFB 003.007\n"
    putStr $ "Requsted Protocol Version: " ++ version
    sendString sock version

    -- Receive number of security types
    (numberOfSecurityTypes:_) <- recvInts sock 1

    -- Receive security types
    securityTypes <- recvString sock numberOfSecurityTypes
    putStrLn $ "Server Security Types: " ++ securityTypes

    -- TODO Actually check security types before blindy choosing
    --send sock (intsToBytestring [1])
    sendInts sock [1]

    -- I don't know why SecurityResult isn't being sent
    -- msg <- recv sock 1

    -- Allow shared desktop
    sendInts sock [1]

    -- Get ServerInit message
    (fbW1:fbW2:
        fbH1:fbH2:
        bitsPerPixel:
        depth:
        bigEndianFlag:
        trueColourFlag:
        redMax1:redMax2:
        blueMax1:blueMax2:
        greenMax1:greenMax2:
        redShift:
        greenShift:
        blueShift:
        _) <- recvInts sock 20
    let framebufferWidth = bytesToInt [fbW1, fbW2]
    let framebufferHeight = bytesToInt [fbH1, fbH2]
    let redMax = bytesToInt [redMax1, redMax2]
    let blueMax = bytesToInt [blueMax1, blueMax2]
    let greenMax = bytesToInt [greenMax1, greenMax2]
    --putStrLn $ "serverInit: " ++ show serverInit
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

    let framebufferUpdateRequest =
            [ 3, 0, 0, 0, 0, 0
            , hiByte framebufferWidth, loByte framebufferWidth
            , hiByte framebufferHeight, loByte framebufferHeight ]
    sendInts sock framebufferUpdateRequest
    (messageType:padding:nR1:nR2:_) <- recvInts sock 4
    let numberofRectangles = bytesToInt [nR1, nR2]
    putStrLn $ "numberofRectangles: " ++ show numberofRectangles

    hold <- getLine

    -- Close socket
    sClose sock

bytestringToInts :: B8.ByteString -> [Int]
bytestringToInts b = map ord (B8.unpack b)

intsToBytestring :: [Int] -> B8.ByteString
intsToBytestring b = B8.pack (map chr b)

recvString :: Socket -> Int -> IO [Char]
recvString s l = fmap B8.unpack (recv s l)

recvInts :: Socket -> Int -> IO [Int]
recvInts s l = fmap bytestringToInts (recv s l)

sendString :: Socket -> String -> IO Int
sendString s l = send s (B8.pack l)

sendInts :: Socket -> [Int] -> IO Int
sendInts s l = send s (intsToBytestring l)

bytesToInt :: [Int] -> Int
bytesToInt [] = 0
bytesToInt b = shiftL (bytesToInt (init b)) 8 .|. (last b)

hiByte :: Int -> Int
hiByte b = shiftR (b .&. 0xFF00) 8

loByte :: Int -> Int
loByte b = b .&. 0xFF
