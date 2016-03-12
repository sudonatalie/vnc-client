\section{Client.lhs}

> module RFB.Client (Box(..), RFBFormat(..), runVNCClient, setEncodings,
>                    setPixelFormat) where

> import RFB.Network
> import Control.Concurrent (forkIO, threadDelay)
> import Control.Monad.Reader
> import Data.Bits ((.|.), (.&.), shiftL, shiftR)
> import Data.Int (Int32)
> import Foreign.C.Types (CInt)
> import Graphics.X11.Xlib
> import Graphics.X11.Xlib.Extras
> import Network.Socket (Socket)

> data RFBFormat =  RFBFormat
>                   { encodingTypes   :: [Int]
>                   , bitsPerPixel    :: Int
>                   , depth           :: Int
>                   , bigEndianFlag   :: Int
>                   , trueColourFlag  :: Int
>                   , redMax          :: Int
>                   , greenMax        :: Int
>                   , blueMax         :: Int
>                   , redShift        :: Int
>                   , greenShift      :: Int
>                   , blueShift       :: Int
>                   } deriving (Show)

> data Box =  Box
>             { x  :: Int
>             , y  :: Int
>             , w  :: Int
>             , h  :: Int
>             } deriving (Show)

> data VNCDisplayWindow =  VNCDisplayWindow
>                          { display  :: Display  -- X display
>                          , rootw    :: Window   -- root window
>                          , win      :: Window   -- output window
>                          , pixmap   :: Pixmap   -- image buffer
>                          , wingc    :: GC       -- graphics contexts
>                          , pixgc    :: GC
>                          , width    :: Dimension
>                          , height   :: Dimension
>                          , bpp      :: Int
>                          }

> data Environment = Environment
>                    { sock        :: Socket
>                    , framebuffer :: Box
>                    , xWindow     :: VNCDisplayWindow
>                    , leftOffset  :: Int
>                    , topOffset   :: Int
>                    }

> type VNCClient = ReaderT Environment IO

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

> refreshWindow :: VNCClient ()
> refreshWindow = do
>     env <- ask
>     (_:n1:n2:_) <- liftIO $ recvInts (sock env) 3
>     sequence_ $ replicate (bytesToInt [n1, n2]) handleRectangleHeader
>     swapBuffer
>     liftIO $ flush (display . xWindow $ env)

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

> sendKeyEvent :: Socket -> Bool -> Int -> IO ()
> sendKeyEvent sock keyPos key = let downFlag = if keyPos then 1 else 0
>                                in sendInts sock ( 4 -- message type
>                                                  :downFlag
>                                                  :0:0
>                                                  :intToBytes 4 key)
>                                   >> return ()

\subsection{Graphics Functions}

Get the header information for each rectangle to be drawn and draw the
rectangle.

> handleRectangleHeader :: VNCClient ()
> handleRectangleHeader = do
>     env <- ask
>     (x1:x2:
>      y1:y2:
>      w1:w2:
>      h1:h2:
>      e1:e2:e3:e4:
>      _) <- liftIO $ recvInts (sock env) 12
>     let rect = Box  { x = bytesToInt [x1, x2]
>                     , y = bytesToInt [y1, y2]
>                     , w = bytesToInt [w1, w2]
>                     , h = bytesToInt [h1, h2] }
>     displayRectangle (fromIntegral . bytesToInt $ [e1, e2, e3, e4])
>         (x rect - leftOffset env) (y rect - topOffset env) (w rect) (h rect)

Choose which decoding function to use for the rectangle.

> displayRectangle ::  Int32 -> Int -> Int -> Int -> Int -> VNCClient ()
> displayRectangle a x y w h = (decodeFunc a) x y w h
>     where
>       decodeFunc :: Int32 -> (Int -> Int -> Int -> Int -> VNCClient ())
>       decodeFunc  0     = decodeRAW
>       decodeFunc  1     = decodeCopyRect
>       decodeFunc  2     = decodeRRE
>       decodeFunc (-239) = pseudoDecodeCursor
>       decodeFunc  _     = \_ _ _ _ -> return ()

\subsubsection{Image Decoding Functions}

The following encoding types are supported, listed by their numeric identifier:
\begin{itemize}
  \item 0 - RAW
  \item 1 - CopyRect
  \item 2 - RRE
\end{itemize}
The following are partially implemented, but not ready use in the application:
\begin{itemize}
  \item (-239) - cursor pseudo-encoding
\end{itemize}

> decodeRAW :: Int -> Int -> Int -> Int -> VNCClient ()
> decodeRAW x y w h = do
>     env <- ask
>     let colors = recvColorList (w*h)
>     sequence_ $ zipWith (\ (a,b) c -> displayPixel a b =<< c) positions colors
>     where
>       positions = [(x,y) | y <- [y..(y+h-1)], x <- [x..(x+w-1)]]

> decodeCopyRect :: Int -> Int -> Int -> Int -> VNCClient ()
> decodeCopyRect x y w h = do
>     env <- ask
>     let xWin = xWindow env
>     srcx1:srcx2:srcy1:srcy2:_ <- liftIO $ recvInts (sock env) 4
>     liftIO $ copyArea (display xWin) (pixmap xWin) (pixmap xWin) (pixgc xWin)
>         (fromIntegral $ bytesToInt [srcx1, srcx2] - leftOffset env)
>         (fromIntegral $ bytesToInt [srcy1, srcy2] - topOffset env)
>         (fromIntegral w) (fromIntegral h) (fromIntegral x) (fromIntegral y)

> decodeRRE :: Int -> Int -> Int -> Int -> VNCClient ()
> decodeRRE x y w h = do
>     env <- ask
>     s1:s2:s3:s4:_ <- liftIO $ recvInts (sock env) 4
>     color <- recvColor
>     drawRect x y w h color
>     sequence_ . replicate (bytesToInt [s1, s2, s3, s4]) $ drawRRESubRect x y

> pseudoDecodeCursor :: Int -> Int -> Int -> Int -> VNCClient ()
> pseudoDecodeCursor x y w h = do
>     env <- ask
>     colors  <- sequence $ recvColorList (w*h)
>     bitMask <- liftIO $ recvBitMask (sock env) (( (w+7) `div` 8) * h)
>     drawBitmaskedPixels 100 w (((w+7)`div`8)*8) colors bitMask 100 100

\subsubsection{Supporting Functions for Encoding}

Displays a subrectangle for RRE encoding.

> drawRRESubRect :: Int -> Int -> VNCClient ()
> drawRRESubRect x0 y0 = do
>     env <- ask
>     color <- recvColor
>     (x1:x2:
>      y1:y2:
>      w1:w2:
>      h1:h2:
>      _) <- liftIO $ recvInts (sock env) 8
>     let rect = Box  { x = bytesToInt [x1, x2]
>                     , y = bytesToInt [y1, y2]
>                     , w = bytesToInt [w1, w2]
>                     , h = bytesToInt [h1, h2] }
>     drawRect (x0+(x rect)) (y0+(y rect)) (w rect) (h rect) color

Draw pixels based on bitmask data.

> --needs a rewrite
> drawBitmaskedPixels :: Int -> Int -> Int -> [Int] -> [Bool] -> Int -> Int -> VNCClient ()
> drawBitmaskedPixels _  _ _        []        _       _ _ = return ()
> drawBitmaskedPixels x0 w wBitMask colorList bitMask x y = do
>     if x >= x0 + w
>     then if x >= x0 + wBitMask
>         then drawBitmaskedPixels x0 w wBitMask colorList (bitMask) x0 (y+1)
>         else drawBitmaskedPixels x0 w wBitMask colorList (tail bitMask) (x+1) y
>     else if (head bitMask)
>         then do
>             displayPixel x y (head colorList)
>             drawBitmaskedPixels x0 w wBitMask (tail colorList) (tail bitMask) (x+1) y
>         else drawBitmaskedPixels x0 w wBitMask (tail colorList) (tail bitMask) (x+1) y
> --drawBitmaskedPixels xWindow w widthBitMask colors bitMask x y =
> --    sequence_ . zipWith3 (\ (x, y) c bit -> if bit
> --                                            then displayPixel xWindow x y c
> --                                            else return ()
> --                         )
> --                         positions colors $ bits bitMask
> --    where
> --      positions = [(x,y) || y <- [y..], x <- [x..(x+w-1)]]
> --      bits bs = if widthBitMask > w
> --                then (\ (xs, ys) -> xs ++ (bits . drop (widthBitMask - w) $ ys)) $ splitAt w bs
> --                else bitMask

\subsubsection{Recieving Image Data from Server}

Get a list of colors.

> recvColorList :: Int -> [VNCClient Int]
> recvColorList size = replicate size recvColor

Get the color to be drawn. Supports various bit per pixel formats. 24 bpp is
non-standard in the RFB protocol, but we support it because some servers will
accept it.

> recvColor :: VNCClient Int
> recvColor = do
>     env <- ask
>     liftIO $ recvColor' (sock env) (bpp . xWindow $ env)
>     where
>       recvColor' sock 32 = recvInts sock 4 >>= return . bytesToInt . take 3
>       recvColor' sock 24 = recvInts sock 3 >>= return . bytesToInt
>       recvColor' sock 16 = recvInts sock 2 >>= return . bytesToInt
>       recvColor' sock 8  = recvInts sock 1 >>= return . bytesToInt
>       recvColor' _    _  = error "Unsupported bits-per-pixel setting"

Get Bitmask array to describe which pixels in an image are valid.

> recvBitMask :: Socket -> Int -> IO [Bool]
> recvBitMask _ 0 = return []
> recvBitMask sock size = do
>     byte:_ <- recvInts sock 1
>     xs <- recvBitMask sock (size-1)
>     return $ ((byte .&. 0x80)>0):
>              ((byte .&. 0x40)>0):
>              ((byte .&. 0x20)>0):
>              ((byte .&. 0x10)>0):
>              ((byte .&. 0x08)>0):
>              ((byte .&. 0x04)>0):
>              ((byte .&. 0x02)>0):
>              ((byte .&. 0x01)>0):xs
> --recvBitmask' w h = (foldl' (:) $ foldl' (:) getBit ):[]

\subsubsection{Drawing to Screen}

Draw an individual pixel to the buffer

> displayPixel :: Int -> Int -> Int -> VNCClient ()
> displayPixel x y color = do
>     env <- ask
>     let xWin = xWindow env
>     liftIO $ setForeground (display xWin) (pixgc xWin) (fromIntegral color)
>     liftIO $ drawPoint (display xWin) (pixmap xWin) (pixgc xWin)
>         (fromIntegral x) (fromIntegral y)

Draw a filled rectangle to the buffer

> drawRect :: Int -> Int -> Int -> Int -> Int -> VNCClient ()
> drawRect x y 1 1 color = displayPixel x y color
> drawRect x y w h color = do
>     env <- ask
>     let xWin = xWindow env
>     liftIO $ setForeground (display xWin) (pixgc xWin) (fromIntegral color)
>     liftIO $ fillRectangle (display xWin) (pixmap xWin) (pixgc xWin)
>         (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h)

Swap the buffered image to the displayed window. This function allows double
buffering. This reduces the time it takes to draw an update, and eliminates any
tearing effects.

> swapBuffer :: VNCClient ()
> swapBuffer = do
>     env <- ask
>     let xWin = xWindow env
>     liftIO $ copyArea (display xWin) (pixmap xWin) (win xWin) (pixgc xWin)
>                       0 0 (width xWin) (height xWin) 0 0

\subsection{Input Functions}

> data InputEvent = KeyEv Bool KeySym | Other

> eventList :: VNCDisplayWindow -> [IO InputEvent]
> eventList xWindow = repeat $ singleEvent
>   where
>     singleEvent :: IO InputEvent
>     singleEvent = do
>         p <- pending (display xWindow)
>         if p /= 0
>            then allocaXEvent f
>            else threadDelay 100000 >> singleEvent
>     f e = do
>         nextEvent (display xWindow) e
>         ev     <- getEvent e
>         evType <- get_EventType e
>         getEventData e ev evType
>     getEventData :: XEventPtr -> Event -> EventType -> IO InputEvent
>     getEventData ePtr e eType | eType == keyPress   = lookupKeysym (asKeyEvent ePtr) 0 >>= \k -> return $ KeyEv True  k
>                               | eType == keyRelease = lookupKeysym (asKeyEvent ePtr) 0 >>= \k -> return $ KeyEv False k
>     getEventData _ _ _ = return Other

> inputHandler :: VNCClient ()
> inputHandler = do
>     env <- ask
>     let events = eventList (xWindow env)
>     liftIO . sequence_ . fmap (f (sock env)) $ events
>   where
>     f s e = do ev <- e
>                handleEvent s ev

> handleEvent :: Socket -> InputEvent -> IO ()
> handleEvent sock (KeyEv keyPos keySym) = sendKeyEvent sock keyPos (fromIntegral keySym)
> handleEvent _     _                    = return ()
