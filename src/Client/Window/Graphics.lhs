\section{Client.Window.Graphics}

> module Client.Window.Graphics (refreshWindow) where

> import Client.Network
> import Client.Types
> import Control.Monad (liftM, replicateM, replicateM_, when)
> import Data.Binary (decode)
> import Data.Bits ((.&.))
> import Data.ByteString.Lazy as ByteString (cons')
> import qualified Graphics.X11.Xlib as X11

> refreshWindow :: VNCWindow ()
> refreshWindow = do
>     disp <- display . xWindow <$> getWindowInfo
>     runRFB $ recvPadding 1
>     numRectangles :: U16 <- runRFB recvInt
>     replicateM_ (fromIntegral numRectangles) handleRectangleHeader
>     swapBuffer
>     liftIO $ X11.flush disp

\subsection{Header Functions}

Get the header information for each rectangle to be drawn and draw the
rectangle.

> handleRectangleHeader :: VNCWindow ()
> handleRectangleHeader = do
>     env <- getWindowInfo
>     x:y:w:h:_ :: [U16] <- runRFB $ recvInts 4
>     encoding  :: S32   <- runRFB recvInt
>     displayRectangle encoding (x - leftOffset env) (y - topOffset env) w h

Choose which decoding function to use for the rectangle.

> displayRectangle ::  S32 -> U16 -> U16 -> U16 -> U16 -> VNCWindow ()
> displayRectangle enc x y w h = (decodeFunc enc) x y w h
>     where
>       decodeFunc :: S32 -> (U16 -> U16 -> U16 -> U16 -> VNCWindow ())
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
The following are partially implemented, but not ready to use in the
application:
\begin{itemize}
  \item (-239) - cursor pseudo-encoding
\end{itemize}

> decodeRAW :: U16 -> U16 -> U16 -> U16 -> VNCWindow ()
> decodeRAW x y w h = do
>     let colors = recvColorList (fromIntegral w * fromIntegral h)
>     sequence_ $ zipWith (\ (a,b) c -> displayPixel a b =<< c) (positions x y w h) colors

> decodeCopyRect :: U16 -> U16 -> U16 -> U16 -> VNCWindow ()
> decodeCopyRect x y w h = do
>     env <- getWindowInfo
>     let xWin = xWindow env
>     srcX:srcY:_ :: [U16] <- runRFB $ recvInts 2
>     liftIO $ X11.copyArea (display xWin) (pixmap xWin) (pixmap xWin) (pixgc xWin)
>         (fromIntegral $ srcX - leftOffset env) (fromIntegral $ srcY - topOffset env)
>         (fromIntegral w) (fromIntegral h) (fromIntegral x) (fromIntegral y)

> decodeRRE :: U16 -> U16 -> U16 -> U16 -> VNCWindow ()
> decodeRRE x y w h = do
>     numSubrectangles :: U32 <- runRFB recvInt
>     color <- recvColor
>     drawRect x y w h color
>     replicateM_ (fromIntegral numSubrectangles) drawRRESubRect
>   where
>     -- Displays a subrectangle for RRE encoding.
>     drawRRESubRect = do
>         color <- recvColor
>         x':y':w':h':_ :: [U16] <- runRFB $ recvInts 4
>         drawRect (x + x') (y + y') w' h' color

> pseudoDecodeCursor :: U16 -> U16 -> U16 -> U16 -> VNCWindow ()
> pseudoDecodeCursor x y w h = do
>     colors <- sequence $ recvColorList (fromIntegral w * fromIntegral h)
>     bitmask <- recvBitmask w h
>     sequence_ $ zipWith3 (\ (a,b) c bit -> when bit (displayPixel a b c)) (positions x y w h) colors bitmask

\subsubsection{Supporting Functions for Decoding Images}

Generate a list of pixel positions in a rectangular area.

> positions :: U16 -> U16 -> U16 -> U16 -> [(U16,U16)]
> positions x y w h = [(x,y) | y <- [y..(y+h-1)], x <- [x..(x+w-1)]]

\subsection{Recieving Image Data from Server}

Get a list of colors.

> recvColorList :: Int -> [VNCWindow Color]
> recvColorList size = replicate size recvColor

Get the color to be drawn. Supports various bit per pixel formats.

> recvColor :: VNCWindow Color
> recvColor = do
>     bitsPerPixel <- bpp . xWindow <$> getWindowInfo
>     runRFB $ recvColor' bitsPerPixel
>     where
>       recvColor' 32 = do bytes <- recvFixedLengthByte 4
>                          let color = (decode $ ByteString.cons' 0 bytes) :: U32
>                          return $ fromIntegral color
>       recvColor' 24 = do bytes <- recvFixedLengthByte 3
>                          let color = (decode $ ByteString.cons' 0 bytes) :: U32
>                          return $ fromIntegral color
>       recvColor' 16 = (recvInt :: RFB U16) >>= return . fromIntegral
>       recvColor' 8  = (recvInt :: RFB U8)  >>= return . fromIntegral
>       recvColor' _  = error "Unsupported bits-per-pixel setting"

Get Bitmask array to describe which pixels in an image are valid. This function
removes the extra bits that are sent as padding.

> recvBitmask :: U16 -> U16 -> VNCWindow [Bool]
> recvBitmask w h = liftM concat . replicateM (fromIntegral h) $ recvBitRow w
>   where
>     recvBitRow :: U16 -> VNCWindow [Bool]
>     recvBitRow w = do bytes :: [U8] <- runRFB $ recvInts $ ((fromIntegral w)+7)`div`8
>                       return . take (fromIntegral w) . foldr (\a b -> getBits a b) [] $ bytes
>     getBits :: U8 -> ([Bool] -> [Bool])
>     getBits byte xs = ((byte .&. 0x80)>0):
>                       ((byte .&. 0x40)>0):
>                       ((byte .&. 0x20)>0):
>                       ((byte .&. 0x10)>0):
>                       ((byte .&. 0x08)>0):
>                       ((byte .&. 0x04)>0):
>                       ((byte .&. 0x02)>0):
>                       ((byte .&. 0x01)>0):xs

\subsection{Drawing to Screen}

Draw an individual pixel to the buffer

> displayPixel :: U16 -> U16 -> Color -> VNCWindow ()
> displayPixel x y color = do
>     xWin <- xWindow <$> getWindowInfo
>     liftIO $ X11.setForeground (display xWin) (pixgc xWin) color
>     liftIO $ X11.drawPoint (display xWin) (pixmap xWin) (pixgc xWin)
>         (fromIntegral x) (fromIntegral y)

Draw a filled rectangle to the buffer

> drawRect :: U16 -> U16 -> U16 -> U16 -> Color -> VNCWindow ()
> drawRect x y 1 1 color = displayPixel x y color
> drawRect x y w h color = do
>     xWin <- xWindow <$> getWindowInfo
>     liftIO $ X11.setForeground (display xWin) (pixgc xWin) color
>     liftIO $ X11.fillRectangle (display xWin) (pixmap xWin) (pixgc xWin)
>         (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h)

Swap the buffered image to the displayed window. This function allows double
buffering. This reduces the time it takes to draw an update, and eliminates any
tearing effects.

> swapBuffer :: VNCWindow ()
> swapBuffer = do
>     xWin <- xWindow <$> getWindowInfo
>     liftIO $ X11.copyArea (display xWin) (pixmap xWin) (win xWin) (pixgc xWin)
>                       0 0 (width xWin) (height xWin) 0 0
