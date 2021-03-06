\section{Client}

> module Client (launchVNCClient) where

> import Client.Handshake (negotiateProtocolVersion, negotiateSecurity)
> import Client.Messages (setEncodings, setPixelFormat)
> import Client.Network
> import Client.Types
> import Client.Window (runVNCWindow)
> import Control.Monad.Trans.State (modify, evalStateT)
> import Network.Socket (withSocketsDo)
> import System.Exit (exitWith, ExitCode(..))

\subsection{Running VNC Client}

> launchVNCClient :: Interface -> String -> Maybe String -> Options -> IO ()
> launchVNCClient interface host mPwd opts
>     =  withSocketsDo $ evalStateT (runVNCClient host mPwd opts)
>           ClientInfo { sock    = error "Attempting to access socket. Socket connection not yet established."
>                      , version = RFB3_7
>                      , ui      = interface
>                      , verbose = (optVerbose opts)
>                      }

> -- A new function can be created in the future if the process for setting up
> -- a GUI connection diverges from this one.
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
>     s <- liftIO $ connect host port
>     modify $ \a -> a {sock = s}
>     negotiateProtocolVersion supportedRFBVersions
>     negotiateSecurity authentication mPwd
>     clientInit True
>     serverInit <- recvServerInit
>     let framebuffer = verifyFramebuffer (framebufferWidth serverInit) (framebufferHeight serverInit)
>                         width height left top
>     let format = defualtFormat {bitsPerPixel = bpp}
>     status $ "Server Name: " ++ (serverName serverInit)
>     status $ "Framebuffer: " ++ show framebuffer
>     status $ "Encodings supported by client: " ++ show supportedEncodingTypes
>     status $ "Pixel format: " ++ show format
>     runRFB $ setEncodings supportedEncodingTypes
>     runRFB $ setPixelFormat format
>     runVNCWindow framebuffer bpp
>     runRFB disconnect
>     liftIO $ exitWith ExitSuccess
>     return ()

\subsection{Local Functions}

Initialize connection after handshake is complete. ClientInit True allows shared
access to the server, ClientInit False requests exclusive access to the server.

> clientInit :: Bool -> VNCClient ()
> clientInit True = runRFB $ sendInt (1 :: U8)
> clientInit _    = runRFB $ sendInt (0 :: U8)

> recvServerInit :: VNCClient ServerInitMessage
> recvServerInit = do
>     fbWidth:fbHeight:_ :: [U16] <- runRFB $ recvInts 2
>     runRFB $ recvPadding 16 -- server-pixel-format. We should be using this.
>     nameLength :: U32 <- runRFB recvInt
>     serverName <- runRFB $ recvString nameLength
>     return $ ServerInitMessage fbWidth fbHeight undefined serverName

> verifyFramebuffer :: U16 -> U16 -> Maybe U16 -> Maybe U16 -> U16 -> U16->  Box
> verifyFramebuffer serverW serverH mWidth mHeight left top
>   | left >= serverW = error $ "Left offset is greater than server screen width." ++ screenDimensions
>   | top  >= serverH = error $ "Top offset is greater than server screen height." ++ screenDimensions
>   | requestedWidth + left > serverW = error $ "The requested screen horzontal area is outside the bounds of the server Screen." ++ screenDimensions
>   | requestedHeight + top > serverH = error $ "The requested screen vertical area is outside the bounds of the server Screen." ++ screenDimensions
>   | otherwise = Box left top requestedWidth requestedHeight
>   where
>     requestedWidth  = maybe (serverW - left) id mWidth
>     requestedHeight = maybe (serverH - top)  id mHeight
>     screenDimensions = "\nServer Width: " ++ show serverW ++ " pixels\nServer Height: " ++ show serverH ++ " pixels"

\subsection{Default Values}

> supportedRFBVersions = [RFB3_7]

These are the pixel encodings supported in Client.Window.Graphics.

> supportedEncodingTypes :: [S32]
> supportedEncodingTypes = [1,2,0] -- in order of priority

> defualtFormat = PixelFormat
>     { bitsPerPixel   = 24
>     , depth          = 24
>     , bigEndianFlag  = 0
>     , trueColourFlag = 1
>     , redMax         = 255
>     , greenMax       = 255
>     , blueMax        = 255
>     , redShift       = 0
>     , greenShift     = 8
>     , blueShift      = 16
>     }
