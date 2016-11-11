\section{Client.Window.Graphics}

> module Client.Window.Graphics (refreshWindow) where

> import Client.Network
> import Client.Types
> import Control.Monad.Reader
> import Data.Bits ((.&.))
> import Data.Int (Int32)
> import Graphics.X11.Xlib
> import Network.Socket (Socket)

> refreshWindow :: VNCClient ()
> refreshWindow = do
>     env <- ask
>     (_:n1:n2:_) <- liftIO $ recvInts (sock env) 3
>     sequence_ $ replicate (bytesToInt [n1, n2]) handleRectangleHeader
>     swapBuffer
>     liftIO $ flush (display . xWindow $ env)

\subsection{Header Functions}

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

\subsection{Image Decoding Functions}

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
> decodeRRE x' y' w' h' = do
>     env <- ask
>     s1:s2:s3:s4:_ <- liftIO $ recvInts (sock env) 4
>     color <- recvColor
>     drawRect x' y' w' h' color
>     sequence_ . replicate (bytesToInt [s1, s2, s3, s4]) $ drawRRESubRect x' y'
>   where
>     -- Displays a subrectangle for RRE encoding.
>     drawRRESubRect :: Int -> Int -> VNCClient ()
>     drawRRESubRect x0 y0 = do
>         env <- ask
>         color <- recvColor
>         (x1:x2:
>          y1:y2:
>          w1:w2:
>          h1:h2:
>          _) <- liftIO $ recvInts (sock env) 8
>         let rect = Box  { x = bytesToInt [x1, x2]
>                         , y = bytesToInt [y1, y2]
>                         , w = bytesToInt [w1, w2]
>                         , h = bytesToInt [h1, h2] }
>         drawRect (x0+(x rect)) (y0+(y rect)) (w rect) (h rect) color

> pseudoDecodeCursor :: Int -> Int -> Int -> Int -> VNCClient ()
> pseudoDecodeCursor x y w h = do
>     env <- ask
>     colors  <- sequence $ recvColorList (w*h)
>     bitMask <- liftIO $ recvBitMask (sock env) (( (w+7) `div` 8) * h)
>     drawBitmaskedPixels 100 w (((w+7)`div`8)*8) colors bitMask 100 100

\subsubsection{Supporting Functions for Decoding images}

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

\subsection{Recieving Image Data from Server}

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

\subsection{Drawing to Screen}

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
