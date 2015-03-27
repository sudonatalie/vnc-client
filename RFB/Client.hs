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

{-data Pixel = Pixel
	{ r :: Int
	, g :: Int
	, b :: Int
	} deriving (Show)
-}

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

refreshWindow :: Socket -> Box -> Display -> Window -> GC -> Window -> GC -> Int -> Int  -> Int  -> IO ()
refreshWindow _ _ _ _ _ _ _ _ _ 0 = return ()
refreshWindow sock framebuffer display pixmap pixgc win gc w h n = do
	framebufferUpdateRequest sock 1 framebuffer
	--hold <- getLine
	--readable <- isReadable sock
	--if readable
	--	then do
	(a:b:n1:n2:_) <- recvInts sock 4
	--putStrLn $ show a ++ ", " ++ show b ++  ", " ++ show (bytesToInt [n1, n2])
	displayRectangles display pixmap pixgc sock (bytesToInt [n1, n2])
	copyArea display pixmap win pixgc 0 0 (fromIntegral w) ( fromIntegral h) 0 0
	refreshWindow sock framebuffer display pixmap pixgc win gc w h (n-1)
	--	else do
	--		putStrLn $ "Nothing"
	--		refreshWindow sock framebuffer display pixmap pixgc win gc w h (n-1)

	

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

displayRectangles :: Display -> Window -> GC -> Socket -> Int -> IO ()
displayRectangles _ _ _ _ 0 = return ()
displayRectangles display win gc sock n = do
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
	recvAndDisplayPixels display win gc sock ((x rect)) (w rect) (h rect) (x rect ) (y rect ) (bitsPerPixel format)
	displayRectangles display win gc sock (n-1)

--TODO: needs cleaning up
recvAndDisplayPixels :: Display -> Window -> GC -> Socket -> Int -> Int -> Int -> Int -> Int -> Int -> IO ()
recvAndDisplayPixels _ _ _ _ _ _ 0 _ _ _ = return ()
recvAndDisplayPixels display win gc sock x0 w h x y 24 = do
	r:g:b:_ <- recvInts sock 3
	displayPixel display win gc x y r g b
	if ((x+1) >= x0+w)
		then recvAndDisplayPixels display win gc sock x0 w (h-1) x0 (y+1) 24
		else recvAndDisplayPixels display win gc sock x0 w h (x+1) y 24
recvAndDisplayPixels display win gc sock x0 w h x y 32 = do
	r:g:b:_:_ <- recvInts sock 4
	displayPixel display win gc x y r g b
	if ((x+1) >= w)
		then recvAndDisplayPixels display win gc sock x0 w (h-1) x0 (y+1) 32
		else recvAndDisplayPixels display win gc sock x0 w h (x+1) y 32
recvAndDisplayPixels display win gc sock x0 w h x y 16= do
	f:l:_ <- recvInts sock 2
	displayPixel display win gc x y 0 f l
	if ((x+1) >= w)
		then recvAndDisplayPixels display win gc sock x0 w (h-1) x0 (y+1) 16
		else recvAndDisplayPixels display win gc sock x0 w h (x+1) y 16
recvAndDisplayPixels display win gc sock x0 w h x y 8 = do
	byte:_ <- recvInts sock 1
	displayPixel display win gc x y 0 0 byte
	if ((x+1) >= w)
		then recvAndDisplayPixels display win gc sock x0 w (h-1) x0 (y+1) 8
		else recvAndDisplayPixels display win gc sock x0 w h (x+1) y 8
recvAndDisplayPixels _ _ _ _ _ _ _ _ _ _ = return ()

displayPixel :: Display -> Window -> GC -> Int -> Int -> Int -> Int -> Int -> IO ()
displayPixel display win gc x y r g b = do
	setForeground display gc (fromIntegral (bytesToInt [r, g, b]))
	drawPoint display win gc (fromIntegral x) (fromIntegral y)
