\section{Client.Window}

> module Client.Window (runVNCClient) where

> import Client.Messages (framebufferUpdateRequest)
> import Client.Network
> import Client.Types
> import Client.Window.Graphics (refreshWindow)
> import Client.Window.Input (inputHandler)
> import Data.Bits ((.|.))
> import Control.Concurrent (forkIO)
> import Control.Monad.Trans.Reader (runReaderT)
> import Graphics.X11.Xlib
> import Network.Socket (Socket)

\subsection{Main VNC Client functions}

Set up and create an X11 display window.

> createVNCDisplay :: U8 -> U16 -> U16 -> U16 -> U16 -> IO VNCDisplayWindow
> createVNCDisplay bpp x y w h = do
>     display <- openDisplay ""
>     let defaultX   = defaultScreen display
>         border     = blackPixel display defaultX
>         background = blackPixel display defaultX
>     rootw <- rootWindow display defaultX
>     win <- createSimpleWindow display rootw (fromIntegral x) (fromIntegral y)
>         (fromIntegral w) (fromIntegral h) 0 border background
>     let eventMask = keyPressMask.|.keyReleaseMask
>     selectInput display win eventMask
>     setTextProperty display win "VNC Client" wM_NAME
>     mapWindow display win
>     gc <- createGC display win
>     pixmap <-  createPixmap display rootw (fromIntegral w) (fromIntegral h)
>                (defaultDepthOfScreen (defaultScreenOfDisplay display))
>     pixgc <- createGC display pixmap
>     let vncDisplay = VNCDisplayWindow  { display  = display
>                                        , rootw    = rootw
>                                        , win      = win
>                                        , pixmap   = pixmap
>                                        , wingc    = gc
>                                        , pixgc    = pixgc
>                                        , width    = (fromIntegral w)
>                                        , height   = (fromIntegral h)
>                                        , bpp      = bpp
>                                        }
>     sync display False
>     return vncDisplay

Close and destroy the X11 display window. Also, empty structures containing
image data (possibly unnecessary).

> destroyVNCDisplay :: VNCDisplayWindow -> IO ()
> destroyVNCDisplay xWindow = do
>     freeGC (display xWindow) (pixgc xWindow)
>     freeGC (display xWindow) (wingc xWindow)
>     freePixmap (display xWindow) (pixmap xWindow)
>     sync (display xWindow) False
>     closeDisplay (display xWindow)

Run the VNC Client: create a diplay window, get the initial framebuffer data,
refresh the display window in a loop, and destroy the window when finished.

> runVNCClient :: Socket -> Box -> U8 -> IO ()
> runVNCClient sock framebuffer bpp = do
>     initThreads -- required for X11 threading
>     xWindow <- createVNCDisplay bpp 0 0 (w framebuffer) (h framebuffer)
>     let env = Environment { sock        = sock
>                           , framebuffer = framebuffer
>                           , xWindow     = xWindow
>                           , leftOffset  = (x framebuffer)
>                           , topOffset   = (y framebuffer)
>                           }
>     runReaderT (framebufferUpdateRequest 0) env
>     message :: U8 <- runRFBWithSocket sock $ recvInt
>     runReaderT (handleServerMessage message) env
>     forkIO $ runReaderT inputHandler env
>     runReaderT vncMainLoop env
>     destroyVNCDisplay xWindow

This is the main loop of the application.

> vncMainLoop :: VNCClient ()
> vncMainLoop = do
>     framebufferUpdateRequest 1
>     message :: U8 <- runRFB recvInt
>     handleServerMessage message
>     vncMainLoop

\subsection{Server to Client Message Handling}

Get a message from the sever, and send it to the right function to handle the
data that will follow. The message types are:
\begin{itemize}
  \item 0 - Graphics update
  \item 1 - Get color map data (not implemented)
  \item 2 - Beep sound
  \item 3 - Cut text from server
\end{itemize}

> handleServerMessage :: U8 -> VNCClient ()
> handleServerMessage 0 = refreshWindow
> handleServerMessage 1 = error "Server message 'SetColourMapEntries' has not been implemented."
> handleServerMessage 2 = liftIO $ putStr "\a" -- Beep
> handleServerMessage 3 = serverCutText
> handleServerMessage _ = error "Unsupported server message."

> serverCutText :: VNCClient ()
> serverCutText = do
>     runRFB $ recvPadding 3
>     len :: U32 <- runRFB recvInt
>     cutText <- runRFB $ recvString len
>     -- we should be copying cutText to the clipboard here
>     -- but we will print instead
>     liftIO $ putStrLn cutText
