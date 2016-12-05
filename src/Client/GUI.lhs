\section{Client.GUI}

> module Client.GUI (Client.GUI.connect) where

> import Client.Messages (setEncodings, setPixelFormat)
> import Client.Network
> import Client.Security (hashVNCPassword)
> import Client.Types
> import Client.Window (runVNCClient)
> import Control.Exception (bracket_)
> import Network.Socket (withSocketsDo)
> import System.Exit (exitWith, ExitCode(..))
> import System.IO (hGetEcho, hFlush, hSetEcho, stdin, stdout)

> connect :: String -> Options -> String -> IO ()
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

>     sock <- Client.Network.connect host port

Check for VNC server

>     msg <- runRFBWithSocket sock $ recvString 12

Choose version number

>     let version = "RFB 003.007\n"
>     runRFBWithSocket sock $ sendString version

Receive security types

>     numSecurityTypes :: U8 <- runRFBWithSocket sock $ recvInt
>     securityTypes :: [U8] <- runRFBWithSocket sock $ recvInts (fromIntegral numSecurityTypes)

Choose security type and submit hashed password.

>     if (noAuth)
>       then do
>         runRFBWithSocket sock $ sendInt (1 :: U8)
>         return ()
>       else do
>         runRFBWithSocket sock $ sendInt (2 :: U8)
>         challenge :: [U8] <- runRFBWithSocket sock $ recvInts 16
>         runRFBWithSocket sock . sendInts . packIntList $ hashVNCPassword password challenge
>         securityResult :: U32 <- runRFBWithSocket sock $ recvInt
>         return ()

Allow shared desktop

>     runRFBWithSocket sock $ sendInt (1 :: U8)

Get server initialisation message

>     w':h':_ :: [U16] <- runRFBWithSocket sock $ recvInts 2
>     runRFBWithSocket sock $ recvPadding 16 -- server-pixel-format. We should be using this.
>     nameLength :: U32 <- runRFBWithSocket sock $ recvInt
>     serverName <- runRFBWithSocket sock $ recvString nameLength

>     let framebuffer = Box  { x = left
>                            , y = top
>                            , w = case width of
>                                      Just w -> w
>                                      Nothing -> w' - left
>                            , h = case height of
>                                      Just h -> h
>                                      Nothing -> h' - top
>                            }

>     runRFBWithSocket sock $ setEncodings format
>     runRFBWithSocket sock $ setPixelFormat format

Run the VNC Client. This will run the X11 display window and communicate back
and forth with the server.

>     runVNCClient sock framebuffer bpp

Close socket

>     runRFBWithSocket sock $ disconnect
>     exitWith ExitSuccess
