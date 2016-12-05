\section{Client.CLI}

> module Client.CLI (Client.CLI.connect) where

> import Client.Network
> import Client.Security (hashVNCPassword)
> import Client.Types
> import Client.Window (runVNCClient, setEncodings, setPixelFormat)
> import Control.Exception (bracket_)
> import Network.Socket (withSocketsDo)
> import System.Exit (exitWith, ExitCode(..))
> import System.IO (hGetEcho, hFlush, hSetEcho, stdin, stdout) 

\subsection{connect function}

> connect :: String -> Options -> IO ()
> connect host Options  { optHelp       = _
>                       , optVerbose    = verbose
>                       , optGraphical  = _
>                       , optNoAuth     = noAuth
>                       , optPort       = port
>                       , optTop        = top
>                       , optLeft       = left
>                       , optWidth      = width
>                       , optHeight     = height
>                       , optBPP        = bpp }
>     = withSocketsDo $ do

>     let format = RFBFormat
>           { encodingTypes   = [1,2,0] -- in order of priority
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

>     status verbose $ "Connecting to " ++ host ++ ":" ++ show port ++ "..."

>     sock <- Client.Network.connect host port

Check for VNC server

>     msg <- runRFBWithSocket sock $ recvString 12
>     status verbose $ "Server Protocol Version: " ++ msg

Choose version number

>     let version = "RFB 003.007\n"
>     status verbose $ "Requsted Protocol Version: " ++ version
>     runRFBWithSocket sock $ sendString version

Receive security types

>     numSecurityTypes :: U8 <- runRFBWithSocket sock $ recvInt
>     securityTypes :: [U8] <- runRFBWithSocket sock $ recvInts (fromIntegral numSecurityTypes)
>     status verbose $ "Server Security Types: " ++ show securityTypes

Choose security type and submit hashed password.

>     -- TODO: handle incorrect password
>     if (noAuth)
>       then do
>         runRFBWithSocket sock $ sendInt (1 :: U8)
>         return ()
>       else do
>         runRFBWithSocket sock $ sendInt (2 :: U8)
>         challenge :: [U8] <- runRFBWithSocket sock $ recvInts 16
>         password <- getPassword
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

>     status verbose $ "Server Name: " ++ serverName
>     status verbose $ "Framebuffer: " ++ show framebuffer
>     status verbose $ "Encoding and pixel format: " ++ show format

>     runRFBWithSocket sock $ setEncodings format
>     runRFBWithSocket sock $ setPixelFormat format

Run the VNC Client. This will run the X11 display window and communicate back
and forth with the server.

>     runVNCClient sock framebuffer bpp

Close socket

>     runRFBWithSocket sock $ disconnect
>     exitWith ExitSuccess


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
