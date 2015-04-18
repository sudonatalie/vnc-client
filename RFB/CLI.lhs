> module RFB.CLI where

> import RFB.Client
> import RFB.Security
> import Network.Socket hiding (send, recv)
> import Network.Socket.ByteString (send, recv)
> --import Graphics.UI.Gtk.General.Clipboard
> import Graphics.X11.Xlib
> import System.Exit (exitWith, ExitCode(..))
> import Control.Concurrent (threadDelay)
> import System.IO 
> import Control.Exception

> connect :: String -> Int -> IO()
> connect host port = withSocketsDo $ do

>         -- Connect to server via socket
>         addrInfo <- getAddrInfo Nothing (Just host) (Just $ show port)
>         let serverAddr = head addrInfo
>         sock <- socket (addrFamily serverAddr) Stream defaultProtocol
>         Network.Socket.connect sock (addrAddress serverAddr)

>         -- Check for VNC server
>         sendInts sock []
>         msg <- recvString sock 12
>         -- TODO Verify version format
>         putStr $ "Server Protocol Version: " ++ msg

>         -- TODO Actually compare version numbers before blindy choosing
>         let version = "RFB 003.007\n"
>         putStr $ "Requsted Protocol Version: " ++ version
>         sendString sock version

>         -- Receive number of security types
>         (numberOfSecurityTypes:_) <- recvInts sock 1

>         -- Receive security types
>         securityTypes <- recvInts sock numberOfSecurityTypes
>         putStrLn $ "Server Security Types: " ++ show securityTypes

>         -- TODO Actually check security types before blindy choosing
>         sendInts sock [2]
>         
>         -- Reveive 16 bytes challenge
>         challenge <- recvInts sock 16
>         -- putStrLn $ "Challenge : " ++ show challenge

>         -- password <- getLine -- Obviously this doesn't work, fix in refactoring
>         password <- getPassword
>         let subkeys = getSubkeys password

>         -- challenge = [125,102,186,0,253,221,4,64,154,249,213,155,187,61,189,28]
>         let (firstHalf, lastHalf) = splitAt (div (length challenge) 2) challenge
>         let cha1 = concatMap decToBin8 firstHalf
>         let cha2 = concatMap decToBin8 lastHalf
>                 
>         let res1 = desEncryption cha1 subkeys
>         let res2 = desEncryption cha2 subkeys
>         let cyphertext = res1 ++ res2
>         -- putStrLn $ "cyphertext : " ++ show cyphertext
>         
>         -- send back encrypted challenge
>         sendInts sock cyphertext
>         
>         -- receive security result. type: U32.
>         msgRes <- recv sock 4
>         -- putStrLn $ "security result : " ++ show msgRes

>         -- Allow shared desktop
>         sendInts sock [1]

>         -- Get ServerInit message
>         (w1:w2:
>          h1:h2:
>          _:_:_:_:_:_:_:_:_:_:_:_:_:_:_:_: -- server-pixel-format
>          l1:l2:l3:l4:
>          _) <- recvInts sock 24

>         let framebuffer = Box { x = 0
>                                                   , y = 0
>                                                   , w = 640--bytesToInt [w1, w2]
>                                                   , h = 640--bytesToInt [h1, h2] 
>                                                   }
>  
>         -- Get ServerName
>         serverName <- recvString sock (bytesToInt [l1, l2, l3, l4])

>         putStrLn $ "Server Name: " ++ serverName
>         putStrLn $ "Framebuffer: " ++ show framebuffer
>         putStrLn $ "Encoding and pixel format: " ++ show format

>         setEncodings sock format
>         setPixelFormat sock format

>         framebufferUpdateRequest sock 0 framebuffer

>         xWindow <- createVNCDisplay 640 0 (w framebuffer) (h framebuffer)
>         --putStrLn "To open display window, press [Enter]..."
>         --hold <- getLine

>         --display screen image
>         (a:b:n1:n2:_) <- recvInts sock 4
>         handleRectangleHeader xWindow sock (bytesToInt [n1, n2])
>         swapBuffer xWindow
>         --clipboardObj <- clipboardGet selectionPrimary
>         --clipboardSetText clipboardObj "hello!"
>         vncMainLoop sock framebuffer xWindow 1000
>         
>         

>         putStrLn "To kill application, press [Enter]..."
>         hold <- getLine
>         freeGC (display xWindow) (pixgc xWindow)
>         freeGC (display xWindow) (wingc xWindow)
>         freePixmap (display xWindow) (pixmap xWindow)

>         sync (display xWindow) False
>         threadDelay (1 * 1000000)
>         exitWith ExitSuccess

>         -- Close socket
>         sClose sock

> getPassword :: IO String
> getPassword = do
>   putStr "Input Password: "
>   hFlush stdout
>   pass <- withEcho False getLine
>   putChar '\n'
>   return pass

> withEcho :: Bool -> IO a -> IO a
> withEcho echo action = do
>   old <- hGetEcho stdin
>   bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action
