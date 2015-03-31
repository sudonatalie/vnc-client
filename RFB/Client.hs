module RFB.Client where

import Network.Socket hiding (send, recv)
import Network.Socket.ByteString (send, recv)
import qualified Data.ByteString.Char8 as B8
import Data.Char (ord, chr)
import Data.Bits

import Graphics.X11.Xlib
import System.Exit (exitWith, ExitCode(..))
import Control.Concurrent (threadDelay)

data RFBFormat = RFBFormat
	{ encodingTypes :: [Int]
	, bitsPerPixel :: Int
	, depth :: Int
	, bigEndianFlag :: Int
	, trueColourFlag :: Int
	, redMax :: Int
	, greenMax :: Int
	, blueMax :: Int
	, redShift :: Int
	, greenShift :: Int
	, blueShift :: Int
	} deriving (Show)

data Box = Box
	{ x :: Int
	, y :: Int
	, w :: Int
	, h :: Int
	} deriving (Show)

data Rectangle = Rectangle
	{ rectangle :: Box
	, pixels :: [Pixel]
	}

format = RFBFormat
	{ encodingTypes = [0]
	, bitsPerPixel = 24
	, depth = 24
	, bigEndianFlag = 0
	, trueColourFlag = 1
	, redMax = 255
	, greenMax = 255
	, blueMax = 255
	, redShift = 0
	, greenShift = 8
	, blueShift =  16 }



data VNCDisplayWindow = VNCDisplayWindow
	{ display :: Display-- X display
	, rootw :: Window 	-- root window
	, win :: Window 	-- output window
	, pixmap :: Pixmap 	-- image buffer
	, wingc :: GC 		-- graphics contexts, might move them out of this module
	, pixgc :: GC
	, width :: Dimension
	, height :: Dimension 
	}

createVNCDisplay :: Int -> Int -> Int -> Int -> IO VNCDisplayWindow
createVNCDisplay x y w h = do
	display <- openDisplay ""
	rootw <- rootWindow display (defaultScreen display)
	win <- mkUnmanagedWindow display (defaultScreenOfDisplay display) rootw (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h)
	setTextProperty display win "VNC Client" wM_NAME
	mapWindow display win
	gc <- createGC display win
	pixmap <- createPixmap display rootw (fromIntegral w) (fromIntegral h) (defaultDepthOfScreen (defaultScreenOfDisplay display))
	pixgc <- createGC display pixmap
	let vncDisplay = VNCDisplayWindow { display = display
									, rootw = rootw
									, win = win
									, pixmap = pixmap
									, wingc = gc
									, pixgc = pixgc
									, width = (fromIntegral w)
									, height = (fromIntegral h)
									}
	return vncDisplay

swapBuffer :: VNCDisplayWindow -> IO ()
swapBuffer xWindow = copyArea (display xWindow) (pixmap xWindow) (win xWindow) (pixgc xWindow) 0 0 (width xWindow) (height xWindow) 0 0

vncMainLoop :: Socket -> Box -> VNCDisplayWindow -> Int -> IO ()
vncMainLoop _ _ _ 0 = return ()
vncMainLoop sock framebuffer xWindow n = do
	framebufferUpdateRequest sock 1 framebuffer
	--
	--
	vncMainLoop sock framebuffer xWindow (n-1)



mkUnmanagedWindow :: Display -> Screen -> Window -> Position -> Position -> Dimension -> Dimension -> IO Window
mkUnmanagedWindow d s rw x y w h = do
	let visual = defaultVisualOfScreen s
	let attrmask = cWOverrideRedirect
	allocaSetWindowAttributes $
		\attributes -> do
		   set_override_redirect attributes True
		   createWindow d rw x y w h 0 (defaultDepthOfScreen s)
						inputOutput visual attrmask attributes

setEncodings :: Socket -> RFBFormat -> IO Int
setEncodings sock format =
	sendInts sock ([ 2     -- message-type
				   , 0 ]    -- padding
				   ++ intToBytes 2 (length (encodingTypes format))
				   ++ concat (map (intToBytes 4) (encodingTypes format)))

setPixelFormat :: Socket -> RFBFormat -> IO Int
setPixelFormat sock format =
	sendInts sock ([ 0         -- message-type
				   , 0, 0, 0   -- padding
				   , bitsPerPixel format
				   , depth format
				   , bigEndianFlag format
				   , trueColourFlag format ]
				   ++ intToBytes 2 (redMax format)
				   ++ intToBytes 2 (greenMax format)
				   ++ intToBytes 2 (blueMax format)
				   ++
				   [ redShift format
				   , greenShift format
				   , blueShift format
				   , 0, 0, 0 ]) -- padding

framebufferUpdateRequest :: Socket -> Int -> Box -> IO Int
framebufferUpdateRequest sock incremental framebuffer =
	sendInts sock ([ 3  -- message-type
				   , incremental]
				   ++ intToBytes 2 (x framebuffer)
				   ++ intToBytes 2 (y framebuffer)
				   ++ intToBytes 2 (w framebuffer)
				   ++ intToBytes 2 (h framebuffer))

refreshWindow :: Socket -> Box -> VNCDisplayWindow -> Int  -> IO ()
refreshWindow _ _ _ 0 = return ()
refreshWindow sock framebuffer xWindow n = do
	framebufferUpdateRequest sock 1 framebuffer
	(a:b:n1:n2:_) <- recvInts sock 4
	--putStrLn $ show a ++ ", " ++ show b ++  ", " ++ show (bytesToInt [n1, n2])
	displayRectangles xWindow sock (bytesToInt [n1, n2])
	swapBuffer xWindow
	refreshWindow sock framebuffer xWindow (n-1)

	

bytestringToInts :: B8.ByteString -> [Int]
bytestringToInts = map ord . B8.unpack

intsToBytestring :: [Int] -> B8.ByteString
intsToBytestring = B8.pack . map chr

recvString :: Socket -> Int -> IO [Char]
recvString s l = fmap B8.unpack (recv s l)

recvInts :: Socket -> Int -> IO [Int]
recvInts s l = fmap bytestringToInts (recv s l)

sendString :: Socket -> String -> IO Int
sendString s l = send s (B8.pack l)

sendInts :: Socket -> [Int] -> IO Int
sendInts s l = send s (intsToBytestring l)

bytesToInt :: [Int] -> Int
bytesToInt [] = 0
bytesToInt b = shiftL (bytesToInt (init b)) 8 .|. (last b)

intToBytes :: Int -> Int -> [Int]
intToBytes 0 _ = []
intToBytes l 0 = 0 : intToBytes (l-1) 0
intToBytes l b = intToBytes (l-1) (shiftR (b .&. 0xFF00) 8) ++ [ b .&. 0xFF ]

displayRectangles :: VNCDisplayWindow -> Socket -> Int -> IO ()
displayRectangles _ _ 0 = return ()
displayRectangles xWindow sock n = do
	(x1:x2:
	 y1:y2:
	 w1:w2:
	 h1:h2:
	 e1:e2:e3:e4:
	 _) <- recvInts sock 12
	let rect = Box { x = bytesToInt [x1, x2]
				   , y = bytesToInt [y1, y2]
				   , w = bytesToInt [w1, w2]
				   , h = bytesToInt [h1, h2] }
	--putStrLn $ show rect ++ ", encoding: " ++ show (bytesToInt [e1, e2, e3, e4])
	recvAndDisplayPixels xWindow sock (bitsPerPixel format) (x rect) (w rect) (h rect) (x rect ) (y rect )
	displayRectangles xWindow sock (n-1)

--TODO: needs cleaning up
recvAndDisplayPixels :: VNCDisplayWindow -> Socket -> Int -> Int -> Int -> Int -> Int -> Int -> IO ()
recvAndDisplayPixels _ _ _ _ _ 0 _ _ = return ()
recvAndDisplayPixels xWindow sock bpp x0 w h x y = do
	color <- recvColor sock bpp
	displayPixel xWindow x y color--r g b
	if ((x+1) >= x0+w)
		then recvAndDisplayPixels xWindow sock bpp x0 w (h-1) x0 (y+1)
		else recvAndDisplayPixels xWindow sock bpp x0 w h (x+1) y

recvColor :: Socket -> Int -> IO Int
recvColor sock 32 = do
	r:g:b:_:_ <- recvInts sock 4
	return (bytesToInt [r, g, b])
recvColor sock 24 = do
	r:g:b:_ <- recvInts sock 3
	return (bytesToInt [r, g, b])
recvColor sock 16 = do
	f:l:_ <- recvInts sock 2
	return (bytesToInt [f, l])
recvColor sock 8 = do
	color:_ <- recvInts sock 1
	return color
recvColor _ _ = return 0

displayPixel :: VNCDisplayWindow -> Int -> Int -> Int -> IO ()
displayPixel xWindow x y color = do
	setForeground (display xWindow) (pixgc xWindow) (fromIntegral color)
	drawPoint (display xWindow) (pixmap xWindow) (pixgc xWindow) (fromIntegral x) (fromIntegral y)
