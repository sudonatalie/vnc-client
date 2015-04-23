\section{GUI.lhs}

> module RFB.GUI where

> import Data
> import RFB.Client
> import RFB.Security
> import Network.Socket hiding (send, recv)
> import Network.Socket.ByteString (send, recv)

> import Graphics.X11.Xlib
> import System.Exit (exitWith, ExitCode(..))
> import Control.Concurrent (threadDelay)

> connect :: String -> Options -> String -> IO()
> connect host Options  { optHelp       = _
>                       , optVerbose    = _
>                       , optGraphical  = _
>                       , optNoAuth     = noAuth
>                       , optPort       = port
>                       , optTop        = top
>                       , optLeft       = left
>                       , optWidth      = width
>                       , optHeight     = height }
>     password = withSocketsDo $ do

Connect to server via socket

>     addrInfo <- getAddrInfo Nothing (Just host) (Just $ show port)
>     let serverAddr = head addrInfo
>     sock <- socket (addrFamily serverAddr) Stream defaultProtocol
>     Network.Socket.connect sock (addrAddress serverAddr)

Check for VNC server

>     sendInts sock []
>     msg <- recvString sock 12

Choose version number

>     let version = "RFB 003.007\n"
>     sendString sock version

Receive number of security types

>     (numberOfSecurityTypes:_) <- recvInts sock 1

Receive security types

>     securityTypes <- recvInts sock numberOfSecurityTypes

Choose security type

>     if (noAuth)
>       then do
>         sendInts sock [1]
>         return ()
>       else do
>         sendInts sock [2]

Reveive 16 byte challenge

>         challenge <- recvInts sock 16

Hash password with cypher

>         let subkeys = getSubkeys password

>         let (firstHalf, lastHalf) = splitAt (div (length challenge) 2) challenge
>         let cha1 = concatMap decToBin8 firstHalf
>         let cha2 = concatMap decToBin8 lastHalf
>                 
>         let res1 = desEncryption cha1 subkeys
>         let res2 = desEncryption cha2 subkeys
>         let cyphertext = res1 ++ res2

Send back encrypted challenge

>         sendInts sock cyphertext

Receive security result. type: U32.

>         msgRes <- recv sock 4
>         return ()

Allow shared desktop

>     sendInts sock [1]

Get server initialisation message

>     (w1:w2:
>      h1:h2:
>      _:_:_:_:_:_:_:_:_:_:_:_:_:_:_:_: -- server-pixel-format
>      l1:l2:l3:l4:
>      _) <- recvInts sock 24

>     let framebuffer = Box  { x = left
>                            , y = top
>                            , w = case width of
>                                      Just w -> w
>                                      Nothing -> bytesToInt [w1, w2] - left
>                            , h = case height of
>                                      Just h -> h
>                                      Nothing -> bytesToInt [h1, h2] - top
>                            }

Get server name

>     serverName <- recvString sock (bytesToInt [l1, l2, l3, l4])

>     setEncodings sock format
>     setPixelFormat sock format

>     framebufferUpdateRequest sock 0 framebuffer

>     xWindow <- createVNCDisplay 0 0 (w framebuffer) (h framebuffer)
>     
>     (_:_:n1:n2:_) <- recvInts sock 4
>     handleRectangleHeader xWindow sock (bytesToInt [n1, n2]) (x framebuffer) (y framebuffer)
>     swapBuffer xWindow
>     
>     vncMainLoop sock framebuffer xWindow (x framebuffer) (y framebuffer)

>     putStrLn "To kill application, press [Enter]..."
>     hold <- getLine

>     freeGC (display xWindow) (pixgc xWindow)
>     freeGC (display xWindow) (wingc xWindow)
>     freePixmap (display xWindow) (pixmap xWindow)

>     sync (display xWindow) False
>     threadDelay (1 * 1000000)
>     exitWith ExitSuccess

Close socket

>     sClose sock
