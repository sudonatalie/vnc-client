\section{Client}

> module Client (launchVNCClient) where

> import Client.Messages (setEncodings, setPixelFormat)
> import Client.Network
> import Client.Security (hashVNCPassword)
> import Client.Types
> import Client.Window (runVNCWindow)
> import Control.Exception (bracket_)
> import Control.Monad.Trans.State (get, modify, evalStateT)
> import Network.Socket (withSocketsDo)
> import System.Exit (exitWith, ExitCode(..))
> import System.IO (hGetEcho, hFlush, hSetEcho, stdin, stdout) 

\subsection{Running VNC Client}

> launchVNCClient :: Interface -> String -> Maybe String -> Options -> IO ()
> launchVNCClient interface host mPwd opts
>     =  withSocketsDo $ evalStateT (runVNCClient host mPwd opts)
>           ClientInfo { sock    = undefined
>                      , version = RFB3_7
>                      , ui      = interface
>                      , verbose = (optVerbose opts)
>                      }

> -- A new function can be created in the future is process for setting up a GUI
> -- connection changes.
> runVNCClient :: String -> Maybe String -> Options -> VNCClient ()
> runVNCClient host mPwd Options  { optHelp       = _
>                                 , optVerbose    = _
>                                 , optGraphical  = _
>                                 , optAuth       = authentication
>                                 , optPort       = port
>                                 , optTop        = top
>                                 , optLeft       = left
>                                 , optWidth      = width
>                                 , optHeight     = height
>                                 , optBPP        = bpp
>                                 }
>     = do
>     status $ "Connecting to " ++ host ++ ":" ++ show port ++ "..."
>     temp <- get
>     s <- liftIO $ connect host port
>     modify $ \a -> a {sock = s}

Get Server Protocol Version

>     msg <- runRFB $ recvString 12
>     status $ "Server Protocol Version: " ++ msg

Choose version number

>     let version = "RFB 003.007\n"
>     status $ "Requsted Protocol Version: " ++ version
>     runRFB $ sendString version

Receive security types

>     numSecurityTypes :: U8   <- runRFB recvInt
>     securityTypes    :: [U8] <- runRFB $ recvInts (fromIntegral numSecurityTypes)
>     status $ "Server Security Types: " ++ show securityTypes

Choose security type and submit hashed password.

>     authenticateUser authentication mPwd

Allow shared desktop

>     runRFB $ sendInt (1 :: U8)

Get server initialisation message

>     w':h':_ :: [U16] <- runRFB $ recvInts 2
>     runRFB $ recvPadding 16 -- server-pixel-format. We should be using this.
>     nameLength :: U32 <- runRFB recvInt
>     serverName <- runRFB $ recvString nameLength

>     let framebuffer = Box  { x = left
>                            , y = top
>                            , w = case width of
>                                      Just w -> w
>                                      Nothing -> w' - left
>                            , h = case height of
>                                      Just h -> h
>                                      Nothing -> h' - top
>                            }

>     let format = defualtFormat {bitsPerPixel = bpp}

>     status $ "Server Name: " ++ serverName
>     status $ "Framebuffer: " ++ show framebuffer
>     status $ "Encoding and pixel format: " ++ show format

>     runRFB $ setEncodings format
>     runRFB $ setPixelFormat format

Run the VNC Client. This will run the X11 display window and communicate back
and forth with the server.

>     runVNCWindow framebuffer bpp

Close socket and exit

>     runRFB disconnect
>     liftIO $ exitWith ExitSuccess
>     return ()

\subsection{Local Functions}

> authenticateUser :: SecurityType -> Maybe String -> VNCClient U32
> authenticateUser secType Nothing = do 
>     interface <- ui <$> getClientInfo
>     case interface of
>       CLI -> do password <- liftIO getPassword
>                 result <- authenticateUser' secType password
>                 case result of
>                   0 -> return 0
>                   1 -> liftIO (putStrLn "Invalid Password.") >> authenticateUser secType Nothing
>                   2 -> error "Authenication failed: too many password attempts."
>                   _ -> error "Malformed security result."
>       GUI -> error "no password supplied"
> authenticateUser secType (Just password) = authenticateUser' secType password

> authenticateUser' :: SecurityType -> String -> VNCClient U32
> authenticateUser' NoAuth _ = runRFB $ sendInt (1 :: U8) >> return 0
> authenticateUser' VNCAuth password = do
>     runRFB $ sendInt (2 :: U8)
>     challenge :: [U8] <- runRFB $ recvInts 16
>     runRFB $ sendInts . packIntList $ hashVNCPassword password challenge
>     securityResult :: U32 <- runRFB recvInt
>     return securityResult

> getPassword :: IO String
> getPassword = do
>     putStr "Input Password: "
>     hFlush stdout
>     pass <- withEcho False getLine
>     putChar '\n'
>     return pass
>   where
>     withEcho :: Bool -> IO a -> IO a
>     withEcho echo action = do
>       old <- hGetEcho stdin
>       bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action

\subsection{Default Values}

> supportedRFBVersions = [RFB3_7]

> defualtFormat = RFBFormat
>     { encodingTypes   = [1,2,0] -- in order of priority
>     , bitsPerPixel    = 24
>     , depth           = 24
>     , bigEndianFlag   = 0
>     , trueColourFlag  = 1
>     , redMax          = 255
>     , greenMax        = 255
>     , blueMax         = 255
>     , redShift        = 0
>     , greenShift      = 8
>     , blueShift       = 16
>     }