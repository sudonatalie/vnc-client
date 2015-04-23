\section{Client.lhs}

> module RFB.Client where

> import Network.Socket hiding (send, recv)
> import Network.Socket.ByteString (send, recv)
> import qualified Data.ByteString.Char8 as B8
> import Data.Char (ord, chr)
> import Data.Bits
> import Graphics.X11.Xlib
> import System.Exit (exitWith, ExitCode(..))
> import Control.Concurrent (threadDelay)

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

> format =  RFBFormat
>           { encodingTypes   = [1, 0] -- in order of priority
>           , bitsPerPixel    = 24
>           , depth           = 24
>           , bigEndianFlag   = 0
>           , trueColourFlag  = 1
>           , redMax          = 255
>           , greenMax        = 255
>           , blueMax         = 255
>           , redShift        = 0
>           , greenShift      = 8
>           , blueShift       = 16 }

> data VNCDisplayWindow =  VNCDisplayWindow
>                          { display  :: Display  -- X display
>                          , rootw    :: Window   -- root window
>                          , win      :: Window   -- output window
>                          , pixmap   :: Pixmap   -- image buffer
>                          , wingc    :: GC       -- graphics contexts
>                          , pixgc    :: GC
>                          , width    :: Dimension
>                          , height   :: Dimension 
>                          }

> createVNCDisplay :: Int -> Int -> Int -> Int -> IO VNCDisplayWindow
> createVNCDisplay x y w h = do
>     display <- openDisplay ""
>     let defaultX = defaultScreen display
>         border = blackPixel display defaultX
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
>                                                     , rootw    = rootw
>                                                     , win      = win
>                                                     , pixmap   = pixmap
>                                                     , wingc    = gc
>                                                     , pixgc    = pixgc
>                                                     , width    = (fromIntegral w)
>                                                     , height   = (fromIntegral h)
>                                                     }
>     return vncDisplay

> swapBuffer :: VNCDisplayWindow -> IO ()
> swapBuffer xWindow =  copyArea (display xWindow) (pixmap xWindow) (win xWindow)
>                       (pixgc xWindow) 0 0 (width xWindow) (height xWindow) 0 0

> vncMainLoop :: Socket -> Box -> VNCDisplayWindow -> Int -> Int -> IO ()
> vncMainLoop sock framebuffer xWindow l t = do
>     framebufferUpdateRequest sock 1 framebuffer
>     message:_ <-recvInts sock 1
>     handleServerMessage message sock xWindow l t
>     vncMainLoop sock framebuffer xWindow l t

> handleServerMessage :: Int -> Socket -> VNCDisplayWindow -> Int -> Int -> IO ()
> handleServerMessage 0 sock xWindow l t = refreshWindow sock xWindow l t
> handleServerMessage 2 _    _       _ _ = putStr "\a"
> handleServerMessage 3 sock _       _ _ = return () -- updateClipboard sock
> handleserverMessage _ _    _       _ _ = return ()

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

> framebufferUpdateRequest :: Socket -> Int -> Box -> IO Int
> framebufferUpdateRequest sock incremental framebuffer =
>     sendInts sock (  [ 3  -- message-type
>                      , incremental]
>                      ++ intToBytes 2 (x framebuffer)
>                      ++ intToBytes 2 (y framebuffer)
>                      ++ intToBytes 2 (w framebuffer)
>                      ++ intToBytes 2 (h framebuffer))

> refreshWindow :: Socket -> VNCDisplayWindow -> Int -> Int -> IO ()
> refreshWindow sock xWindow l t = do
>     (_:n1:n2:_) <- recvInts sock 3
>     handleRectangleHeader xWindow sock (bytesToInt [n1, n2]) l t
>     swapBuffer xWindow

> recvFixedLength :: Socket -> Int -> IO B8.ByteString
> recvFixedLength s l = do
>     x <- recv s l
>     if B8.length x < l
>     then if B8.length x == 0
>         then error "Connection Lost" 
>         else do
>             y <- recv s (l - B8.length x)
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
> bytesToInt [] = 0
> bytesToInt b = shiftL (bytesToInt (init b)) 8 .|. (last b)

> intToBytes :: Int -> Int -> [Int]
> intToBytes 0 _  = []
> intToBytes l 0  = 0 : intToBytes (l-1) 0
> intToBytes l b  = intToBytes (l-1) (shiftR (b .&. 0xFF00) 8) ++ [ b .&. 0xFF ]

> handleRectangleHeader :: VNCDisplayWindow -> Socket -> Int -> Int -> Int -> IO ()
> handleRectangleHeader _       _    0  _  _  = return ()
> handleRectangleHeader xWindow sock n  l  t  = do
>     (x1:x2:
>      y1:y2:
>      w1:w2:
>      h1:h2:
>      e1:e2:e3:e4:
>      _) <- recvInts sock 12
>     let rect = Box  { x = bytesToInt [x1, x2]
>                     , y = bytesToInt [y1, y2]
>                     , w = bytesToInt [w1, w2]
>                     , h = bytesToInt [h1, h2] }
>     displayRectangle (bytesToInt [e1, e2, e3, e4]) xWindow sock
>         (bitsPerPixel format) (x rect) (y rect) (w rect) (h rect) l t
>     handleRectangleHeader xWindow sock (n-1) l t

> displayRectangle ::  Int -> VNCDisplayWindow -> Socket ->
>                      Int -> Int -> Int -> Int -> Int -> Int -> Int -> IO ()
> displayRectangle  0  xWindow  sock  bpp  x  y  w  h  l  t  =
>     decodeRAW  xWindow  sock  bpp  x  w  h  x  y  l  t
> displayRectangle  1  xWindow  sock  bpp  x  y  w  h  _  _  =
>     decodeCopyRect  xWindow  sock  x  y  w  h
> displayRectangle  _  _        _     _    _  _  _  _  _  _=
>     return ()

> decodeRAW ::  VNCDisplayWindow -> Socket ->
>               Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> IO ()
> decodeRAW  _        _     _    _   _  0  _  _  _  _  = return ()
> decodeRAW  xWindow  sock  bpp  x0  w  h  x  y  l  t  = do
>     color <- recvColor sock bpp
>     displayPixel xWindow (x-l) (y-t) color
>     if ((x+1) >= x0+w)
>         then decodeRAW xWindow sock bpp x0 w (h-1) x0 (y+1) l t
>         else decodeRAW xWindow sock bpp x0 w h (x+1) y l t

> decodeCopyRect :: VNCDisplayWindow -> Socket -> Int -> Int -> Int -> Int -> IO ()
> decodeCopyRect xWindow sock x y w h = do
>     srcx1:srcx2:srcy1:srcy2:_ <- recvInts sock 4
>     copyArea (display xWindow) (pixmap xWindow) (pixmap xWindow) (pixgc xWindow)
>         (fromIntegral (bytesToInt [srcx1, srcx2]))
>         (fromIntegral (bytesToInt [srcy1, srcy2]))
>         (fromIntegral w) (fromIntegral h) (fromIntegral x) (fromIntegral y)

> recvColor :: Socket -> Int -> IO Int
> recvColor sock 32 = do
>     r:g:b:_:_ <- recvInts sock 4
>     return (bytesToInt [r, g, b])
> recvColor sock 24 = do
>     r:g:b:_ <- recvInts sock 3
>     return (bytesToInt [r, g, b])
> recvColor sock 16 = do
>     f:l:_ <- recvInts sock 2
>     return (bytesToInt [f, l])
> recvColor sock 8 = do
>     color:_ <- recvInts sock 1
>     return color
> recvColor _ _ = return 0

> displayPixel :: VNCDisplayWindow -> Int -> Int -> Int -> IO ()
> displayPixel xWindow x y color = do
>     setForeground (display xWindow) (pixgc xWindow) (fromIntegral color)
>     drawPoint (display xWindow) (pixmap xWindow) (pixgc xWindow)
>         (fromIntegral x) (fromIntegral y)
