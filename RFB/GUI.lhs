\section{GUI.lhs}

> module RFB.GUI (RFB.GUI.connect) where

> import Data
> import RFB.Client
> import RFB.Network
> import RFB.Security (hashVNCPassword)
> import Control.Exception (bracket_)
> import Network.Socket hiding (send, recv)
> import Network.Socket.ByteString (send, recv)
> import System.Exit (exitWith, ExitCode(..))
> import System.IO (hGetEcho, hFlush, hSetEcho, stdin, stdout)

> connect :: String -> Options -> String -> IO()
> connect host Options  { optHelp       = _
>                       , optVerbose    = _
>                       , optGraphical  = _
>                       , optNoAuth     = noAuth
>                       , optPort       = port
>                       , optTop        = top
>                       , optLeft       = left
>                       , optWidth      = width
>                       , optHeight     = height
>                       , optBPP        = bpp }
>     password = withSocketsDo $ do

>     let format = RFBFormat
>           { encodingTypes   = [1, 2, 0] -- in order of priority
>           , bitsPerPixel    = bpp
>           , depth           = 24
>           , bigEndianFlag   = 0
>           , trueColourFlag  = 1
>           , redMax          = 255
>           , greenMax        = 255
>           , blueMax         = 255
>           , redShift        = 0
>           , greenShift      = 8
>           , blueShift       = 16 }

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

Choose security type and submit hashed password.

>     if (noAuth)
>       then do
>         sendInts sock [1]
>         return ()
>       else do
>         sendInts sock [2]
>         challenge <- recvInts sock 16
>         sendInts sock $ hashVNCPassword password challenge
>         msgRes <- recv sock 4 -- security result. type: U32
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

Run the VNC Client. This will run the X11 display window and communicate back
and forth with the server.

>     runVNCClient sock framebuffer bpp

Close socket

>     sClose sock
>     exitWith ExitSuccess
