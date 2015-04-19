\section{CLI.lhs}

> module RFB.CLI where

> import Data
> import RFB.Client
> import RFB.Security
> import Network.Socket hiding (send, recv)
> import Network.Socket.ByteString (send, recv)
> import Graphics.X11.Xlib
> import System.Exit (exitWith, ExitCode(..))
> import Control.Concurrent (threadDelay)
> import System.IO 
> import Control.Exception

\subsection{connect function}

> connect :: String -> Options -> IO()
> connect host Options  { optHelp       = _
>                       , optVerbose    = verbose
>                       , optGraphical  = _
>                       , optNoAuth     = noAuth
>                       , optPort       = port
>                       , optTop        = top
>                       , optLeft       = left
>                       , optWidth      = width
>                       , optHeight     = height }
>     = withSocketsDo $ do

Connect to server via socket

>     status verbose $ "Connecting to " ++ host ++ ":" ++ show port ++ "..."

>     addrInfo <- getAddrInfo Nothing (Just host) (Just $ show port)
>     let serverAddr = head addrInfo
>     sock <- socket (addrFamily serverAddr) Stream defaultProtocol
>     Network.Socket.connect sock (addrAddress serverAddr)

Check for VNC server

>     sendInts sock []
>     msg <- recvString sock 12
>     status verbose $ "Server Protocol Version: " ++ msg

Choose version number

>     let version = "RFB 003.007\n"
>     status verbose $ "Requsted Protocol Version: " ++ version
>     sendString sock version

Receive number of security types

>     (numberOfSecurityTypes:_) <- recvInts sock 1

Receive security types

>     securityTypes <- recvInts sock numberOfSecurityTypes
>     status verbose $ "Server Security Types: " ++ show securityTypes

Choose security type

>     if (noAuth)
>       then do
>         sendInts sock [1]
>         return ()
>       else do
>         sendInts sock [2]

Reveive 16 byte challenge

>         challenge <- recvInts sock 16

Get password from user

>         password <- getPassword

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

>     status verbose $ "Server Name: " ++ serverName
>     status verbose $ "Framebuffer: " ++ show framebuffer
>     status verbose $ "Encoding and pixel format: " ++ show format

>     setEncodings sock format
>     setPixelFormat sock format

>     framebufferUpdateRequest sock 0 framebuffer

>     xWindow <- createVNCDisplay 0 0 (w framebuffer) (h framebuffer)

Display screen image

>     (a:b:n1:n2:_) <- recvInts sock 4
>     handleRectangleHeader xWindow sock (bytesToInt [n1, n2])
>     swapBuffer xWindow
>     vncMainLoop sock framebuffer xWindow 1000

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

\subsection{getPassword function}

> getPassword :: IO String
> getPassword = do
>   putStr "Input Password: "
>   hFlush stdout
>   pass <- withEcho False getLine
>   putChar '\n'
>   return pass

\subsection{withEcho function}

> withEcho :: Bool -> IO a -> IO a
> withEcho echo action = do
>   old <- hGetEcho stdin
>   bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action

\subsection{status function}

Print message about the current status if verbose option is enabled.

> status :: Bool -> String -> IO ()
> status verbose msg = if verbose
>                          then putStrLn msg
>                          else return ()
