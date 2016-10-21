\section{Client.Window}

> module Client.Window (runVNCClient, setEncodings, setPixelFormat) where

> import Client.Network
> import Client.Types
> import Client.Window.Graphics (refreshWindow)
> import Client.Window.Input (inputHandler)
> import Data.Bits ((.|.))
> import Control.Concurrent (forkIO)
> import Control.Monad.Reader
> import Graphics.X11.Xlib
> import Network.Socket (Socket)

\subsection{Main VNC Client functions}

Set up and create an X11 display window.

> createVNCDisplay :: Int -> Int -> Int -> Int -> Int -> IO VNCDisplayWindow
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

> runVNCClient :: Socket -> Box -> Int -> IO ()
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
>     message:_ <- recvInts sock 1
>     runReaderT (handleServerMessage message) env
>     forkIO $ runReaderT inputHandler env
>     runReaderT vncMainLoop env
>     destroyVNCDisplay xWindow

This is the main loop of the application.

> vncMainLoop :: VNCClient ()
> vncMainLoop = do
>     env <- ask
>     framebufferUpdateRequest 1
>     message:_ <- liftIO $ recvInts (sock env) 1
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

> handleServerMessage :: Int -> VNCClient ()
> handleServerMessage 0 = refreshWindow
> handleServerMessage 2 = liftIO $ putStr "\a" -- Beep
> handleServerMessage 3 = serverCutText
> handleServerMessage _ = return ()

> serverCutText :: VNCClient ()
> serverCutText = do
>     env <- ask
>     let s = sock env
>     (_:_:_:l1:l2:l3:l4:_) <- liftIO $ recvInts s 7
>     cutText <- liftIO $ recvString s (bytesToInt [l1, l2, l3, l4])
>     -- we should be copying cutText to the clipboard here
>     -- but we will print instead
>     liftIO $ putStrLn cutText

\subsection{Client to Server Messages}

> setEncodings :: Socket -> RFBFormat -> IO Int
> setEncodings sock format =
>     sendInts sock (  [ 2   -- message-type
>                      , 0]  -- padding
>                      ++ intToBytes 2 (length (encodingTypes format))
>                      ++ concat (map (intToBytes 4) (encodingTypes format)))

> setPixelFormat :: Socket -> RFBFormat -> IO Int
> setPixelFormat sock format =
>     sendInts sock (  [ 0        -- message-type
>                      , 0, 0, 0  -- padding
>                      , bitsPerPixel format
>                      , depth format
>                      , bigEndianFlag format
>                      , trueColourFlag format ]
>                      ++ intToBytes 2 (redMax format)
>                      ++ intToBytes 2 (greenMax format)
>                      ++ intToBytes 2 (blueMax format)
>                      ++
>                      [ redShift format
>                      , greenShift format
>                      , blueShift format
>                      , 0, 0, 0 ]) -- padding

> framebufferUpdateRequest :: Int -> VNCClient Int
> framebufferUpdateRequest incremental = do
>     env <- ask
>     let fb = framebuffer env
>     liftIO $ sendInts (sock env) ( [ 3  -- message-type
>                                    , incremental]
>                                    ++ intToBytes 2 (x fb)
>                                    ++ intToBytes 2 (y fb)
>                                    ++ intToBytes 2 (w fb)
>                                    ++ intToBytes 2 (h fb))
