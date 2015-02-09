module RFB.Client where

import Network.Socket hiding (send, recv)
import Network.Socket.ByteString (send, recv)
import qualified Data.ByteString.Char8 as B8
import Data.Char (ord, chr)
import Data.Bits

import Graphics.X11.Xlib
import System.Exit (exitWith, ExitCode(..))
import Control.Concurrent (threadDelay)

data RFBFormat = RFBFormat
    { encodingTypes :: [Int]
    , bitsPerPixel :: Int
    , depth :: Int
    , bigEndianFlag :: Int
    , trueColourFlag :: Int
    , redMax :: Int
    , greenMax :: Int
    , blueMax :: Int
    , redShift :: Int
    , greenShift :: Int
    , blueShift :: Int
    } deriving (Show)

data Box = Box
    { x :: Int
    , y :: Int
    , w :: Int
    , h :: Int
    } deriving (Show)

data PixelPoint = PixelPoint
    { px :: Int
    , py :: Int
    , r :: Int
    , g :: Int
    , b :: Int
    } deriving (Show)

format = RFBFormat
    { encodingTypes = [0]
    , bitsPerPixel = 32
    , depth = 24
    , bigEndianFlag = 0
    , trueColourFlag = 1
    , redMax = 255
    , greenMax = 255
    , blueMax = 255
    , redShift = 0
    , greenShift = 8
    , blueShift =  16 }

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

    -- TODO Actually compare version numbers before blindy choosing
    sendString sock "RFB 003.007\n"

    -- Receive number of security types
    (numberOfSecurityTypes:_) <- recvInts sock 1

    -- Receive security types
    securityTypes <- recvInts sock numberOfSecurityTypes

    -- TODO Actually check security types before blindy choosing
    sendInts sock [1]

    -- I don't know why SecurityResult isn't being sent
    -- msg <- recv sock 1

    -- Allow shared desktop
    sendInts sock [1]

    -- Get ServerInit message
    (w1:w2:
     h1:h2:
     _:_:_:_:_:_:_:_:_:_:_:_:_:_:_:_: -- server-pixel-format
     l1:l2:l3:l4:
     _) <- recvInts sock 24

    let framebuffer = Box { x = 0
                          , y = 0
                          , w = bytesToInt [w1, w2]
                          , h = bytesToInt [h1, h2] }

    -- Get ServerName
    serverName <- recvString sock (bytesToInt [l1, l2, l3, l4])

    setEncodings sock format
    setPixelFormat sock format

    framebufferUpdateRequest sock 0 framebuffer

    display <- openDisplay ""
    rootw <- rootWindow display (defaultScreen display)
    win <- mkUnmanagedWindow display (defaultScreenOfDisplay display) rootw 0 0 (fromIntegral (w framebuffer)) (fromIntegral (h framebuffer))
    setTextProperty display win "VNC Client" wM_NAME
    mapWindow display win

    (_:_:n1:n2:_) <- recvInts sock 4

    putStrLn "Wait a little while for the buffer to fill, then press [Enter]..."
    hold <- getLine

    -- TODO Kind of almost working, but it only displays a slice of the top...
    displayRectangles display win sock (bytesToInt [n1, n2])

    sync display False
    threadDelay (10 * 1000000)
    exitWith ExitSuccess

    -- Close socket
    sClose sock

displayRectangles :: Display -> Window -> Socket -> Int -> IO ()
displayRectangles _ _ _ 0 = return ()
displayRectangles display win sock n = do
    (x1:x2:
     y1:y2:
     w1:w2:
     h1:h2:
     _) <- recvInts sock 12
    let rect = Box { x = bytesToInt [x1, x2]
                   , y = bytesToInt [y1, y2]
                   , w = bytesToInt [w1, w2]
                   , h = bytesToInt [h1, h2] }
    pixels <- fmap (rawToPixelPoints rect) (recvInts sock (w rect * h rect * ((bitsPerPixel format) `div` 8)))
    displayPixels display win pixels
    displayRectangles display win sock (n-1)

displayPixels :: Display -> Window -> [PixelPoint] -> IO ()
displayPixels display win [] = return ()
displayPixels display win ((PixelPoint x y r g b):pixels) = do
    gc <- createGC display win
    setForeground display gc (fromIntegral (bytesToInt [r, g, b]))
    drawPoint display win gc (fromIntegral x) (fromIntegral y)
    freeGC display gc
    displayPixels display win pixels

mkUnmanagedWindow :: Display -> Screen -> Window -> Position -> Position -> Dimension -> Dimension -> IO Window
mkUnmanagedWindow d s rw x y w h = do
  let visual = defaultVisualOfScreen s
      attrmask = cWOverrideRedirect
  allocaSetWindowAttributes $
         \attributes -> do
           set_override_redirect attributes True
           createWindow d rw x y w h 0 (defaultDepthOfScreen s)
                        inputOutput visual attrmask attributes

setEncodings :: Socket -> RFBFormat -> IO Int
setEncodings sock format =
    sendInts sock ([ 2     -- message-type
                   , 0 ]    -- padding
                   ++ intToBytes 2 (length (encodingTypes format))
                   ++ concat (map (intToBytes 4) (encodingTypes format)))

setPixelFormat :: Socket -> RFBFormat -> IO Int
setPixelFormat sock format =
    sendInts sock ([ 0         -- message-type
                   , 0, 0, 0   -- padding
                   , bitsPerPixel format
                   , depth format
                   , bigEndianFlag format
                   , trueColourFlag format ]
                   ++ intToBytes 2 (redMax format)
                   ++ intToBytes 2 (greenMax format)
                   ++ intToBytes 2 (blueMax format)
                   ++
                   [ redShift format
                   , greenShift format
                   , blueShift format
                   , 0, 0, 0 ]) -- padding

framebufferUpdateRequest :: Socket -> Int -> Box -> IO Int
framebufferUpdateRequest sock incremental framebuffer =
    sendInts sock ([ 3  -- message-type
                   , incremental]
                   ++ intToBytes 2 (x framebuffer)
                   ++ intToBytes 2 (y framebuffer)
                   ++ intToBytes 2 (w framebuffer)
                   ++ intToBytes 2 (h framebuffer))

bytestringToInts :: B8.ByteString -> [Int]
bytestringToInts = map ord . B8.unpack

intsToBytestring :: [Int] -> B8.ByteString
intsToBytestring = B8.pack . map chr

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

intToBytes :: Int -> Int -> [Int]
intToBytes 0 _ = []
intToBytes l 0 = 0 : intToBytes (l-1) 0
intToBytes l b = intToBytes (l-1) (shiftR (b .&. 0xFF00) 8) ++ [ b .&. 0xFF ]

rawToPixelPoints :: Box -> [Int] -> [PixelPoint]
rawToPixelPoints (Box x y w h) pixels = rawToPixelPointsHelper x w x y pixels

rawToPixelPointsHelper :: Int -> Int -> Int -> Int -> [Int] -> [PixelPoint]
rawToPixelPointsHelper _ _ _ _ [] = []
rawToPixelPointsHelper x0 w x y (r:g:b:a:t) = (PixelPoint x y r g b) : if ((x+1) >= w) then rawToPixelPointsHelper x0 w x0 (y+1) t else rawToPixelPointsHelper x0 w (x+1) y t
rawToPixelPointsHelper _ _ _ _ _ = []
