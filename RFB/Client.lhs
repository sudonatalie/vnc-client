\section{Client.lhs}

> module RFB.Client where

> import Network.Socket hiding (send, recv)
> import Network.Socket.ByteString (send, recv)
> import qualified Data.ByteString.Char8 as B8
> import Control.Monad.Reader
> import Data.Bits ((.|.), (.&.), shiftL, shiftR)
> import Data.Char (ord, chr)
> import Data.Int (Int32)
> import Data.List (foldl1')
> import Graphics.X11.Xlib
> import System.Exit (exitWith, ExitCode(..))
> import Control.Concurrent (threadDelay)

Encoding types are listed in order of desired priority:
\begin{itemize}
  \item 1 - CopyRect
  \item 2 - RRE
  \item 0 - RAW
\end{itemize}
Implemented but not in the encodings list:
\begin{itemize}
  \item (-239) - cursor pseudo-encoding
\end{itemize}

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

> data Rectangle =  Rectangle
>                   { rectangle  :: Box
>                   , pixels     :: [Pixel]
>                   }

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


> createVNCDisplay :: Int -> Int -> Int -> Int -> Int -> IO VNCDisplayWindow
> createVNCDisplay bpp x y w h = do
>     display <- openDisplay ""
>     let defaultX   = defaultScreen display
>         border     = blackPixel display defaultX
>         background = blackPixel display defaultX
>     rootw <- rootWindow display defaultX
>     win <- createSimpleWindow display rootw (fromIntegral x) (fromIntegral y)
>         (fromIntegral w) (fromIntegral h) 0 border background
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
>     --let eventMask = keyPressMask.|.keyReleaseMask
>     --selectInput display win eventMask
>     return vncDisplay

Swap the buffered image to the displayed window. This function allows double
buffering. This reduces the time it takes to draw an update, and eliminates any
tearing effects.

> swapBuffer :: VNCClient ()
> swapBuffer = do
>     env <- ask
>     let xWin = xWindow env
>     liftIO $ copyArea (display xWin) (pixmap xWin) (win xWin) (pixgc xWin)
>                       0 0 (width xWin) (height xWin) 0 0

This is the main loop of the application.

> vncMainLoop :: VNCClient ()
> vncMainLoop = do
>     env <- ask
>     framebufferUpdateRequest 1
>     message:_ <- liftIO $ recvInts (sock env) 1
>     handleServerMessage message
>     vncMainLoop

\subsection{RFB Functions}
\subsubsection{Server to Client Messages}

Get a message from the sever, and send it to the right function to handle the
data that will follow after it. The message types are:
\begin{itemize}
  \item 0 - Graphics update
  \item 1 - get color map data (not implemented)
  \item 2 - Beep sound
  \item 3 - cut text from server
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
>     handleRectangleHeader (bytesToInt [n1, n2])
>     swapBuffer

> serverCutText :: VNCClient ()
> serverCutText = do
>     env <- ask
>     let s = sock env
>     (_:_:_:l1:l2:l3:l4:_) <- liftIO $ recvInts s 7
>     cutText <- liftIO $ recvString s (bytesToInt [l1, l2, l3, l4])
>     -- we should be copying cutText to the clipboard here
>     -- but we will print instead
>     liftIO $ putStrLn cutText

\subsubsection{Client to Server Messages}

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

> sendKeyEvent :: Socket -> Bool -> Int -> IO Int
> sendKeyEvent sock True key =
>     sendInts sock ([4 -- message type
>                   , 1
>                   , 0, 0 ]
>                   ++ (intToBytes 4 key))
> sendKeyEvent sock False key =
>     sendInts sock ([4 -- message type
>                   , 0
>                   , 0, 0 ]
>                   ++ (intToBytes 4 key))  

\subsection {Network Functions and Type Convertions}

> recvFixedLength :: Socket -> Int -> IO B8.ByteString
> recvFixedLength s l = do
>     x <- recv s l
>     if B8.length x < l
>     then if B8.length x == 0
>         then error "Connection Lost" 
>         else do
>             y <- recvFixedLength s (l - B8.length x)
>             return (B8.append x y)
>     else return x

> bytestringToInts :: B8.ByteString -> [Int]
> bytestringToInts = map ord . B8.unpack

> intsToBytestring :: [Int] -> B8.ByteString
> intsToBytestring = B8.pack . map chr

> recvString :: Socket -> Int -> IO [Char]
> recvString s l = fmap B8.unpack (recvFixedLength s l)

> recvInts :: Socket -> Int -> IO [Int]
> recvInts s l = fmap bytestringToInts (recvFixedLength s l)

> sendString :: Socket -> String -> IO Int
> sendString s l = send s (B8.pack l)

> sendInts :: Socket -> [Int] -> IO Int
> sendInts s l = send s (intsToBytestring l)

> bytesToInt :: [Int] -> Int
> bytesToInt []  = 0
> bytesToInt [b] = b 
> bytesToInt bs  = foldl1' (\ a b -> shiftL a 8 .|. b) bs

> intToBytes :: Int -> Int -> [Int]
> intToBytes l x = let lsr = \b -> shiftR (b .&. 0xFFFFFFFFFFFFFF00) 8
>                  in reverse . take l . fmap (.&. 0xFF) $ iterate lsr x


\subsection{Graphics Functions}

Get the header information for each rectangle to be drawn.

> handleRectangleHeader :: Int -> VNCClient ()
> handleRectangleHeader 0 = return ()
> handleRectangleHeader n = do
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
>     displayRectangle (fromIntegral (bytesToInt [e1, e2, e3, e4]))
>         (x rect - leftOffset env) (y rect - topOffset env) (w rect) (h rect)
>     handleRectangleHeader (n-1)

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
>     drawRRESubRects x y (bytesToInt [s1, s2, s3, s4])

> pseudoDecodeCursor :: Int -> Int -> Int -> Int -> VNCClient ()
> pseudoDecodeCursor x y w h = do
>     env <- ask
>     colors  <- sequence $ recvColorList (w*h)
>     bitMask <- liftIO $ recvBitMask (sock env) (( (w+7) `div` 8) * h)
>     drawBitmaskedPixels 100 w (((w+7)`div`8)*8) colors bitMask 100 100

\subsubsection{Supporting Functions for Encoding}

Displays the subractangles for RRE encoding.

> drawRRESubRects :: Int -> Int -> Int -> VNCClient ()
> drawRRESubRects _  _  0 = return ()
> drawRRESubRects x0 y0 n = do
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
>     drawRRESubRects x0 y0 (n-1)

Get list of colors.

> recvColorList :: Int -> [VNCClient Int]
> recvColorList size = replicate size recvColor

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
> --      positions = [(x,y) | y <- [y..], x <- [x..(x+w-1)]]
> --      bits bs = if widthBitMask > w
> --                then (\ (xs, ys) -> xs ++ (bits . drop (widthBitMask - w) $ ys)) $ splitAt w bs
> --                else bitMask

\subsubsection{Drawing to Screen}

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
