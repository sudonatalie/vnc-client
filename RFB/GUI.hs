module RFB.GUI where

import RFB.Client
import RFB.Security
import Network.Socket hiding (send, recv)
import Network.Socket.ByteString (send, recv)

connect :: String -> Int -> String -> IO()
connect host port password = withSocketsDo $ do

	-- Connect to server via socket
	addrInfo <- getAddrInfo Nothing (Just host) (Just $ show port)
	let serverAddr = head addrInfo
	sock <- socket (addrFamily serverAddr) Stream defaultProtocol
	Network.Socket.connect sock (addrAddress serverAddr)

	-- Check for VNC server
	sendInts sock []
	msg <- recvString sock 12
	-- TODO Verify version format

	-- TODO Actually compare version numbers before blindy choosing
	let version = "RFB 003.007\n"
	sendString sock version

	-- Receive number of security types
	(numberOfSecurityTypes:_) <- recvInts sock 1

	-- Receive security types
	securityTypes <- recvInts sock numberOfSecurityTypes

	-- TODO Actually check security types before blindy choosing
	sendInts sock [2]
	
	-- Reveive 16 bytes challenge
	challenge <- recvInts sock 16

	let subkeys = getSubkeys password

	-- challenge = [125,102,186,0,253,221,4,64,154,249,213,155,187,61,189,28]
	let cha1 = concatMap decToBin8 (firstHalf challenge)
	let cha2 = concatMap decToBin8 (lastHalf challenge)
		
	let res1 = desEncryption cha1 subkeys
	let res2 = desEncryption cha2 subkeys
	let cyphertext = res1 ++ res2
	
	-- send back encrypted challenge
	sendInts sock cyphertext
	
	-- receive security result. type: U32.
	msgRes <- recv sock 4

	-- Allow shared desktop
	sendInts sock [1]

	-- Get ServerInit message
	(w1:w2:
	 h1:h2:
	 _:_:_:_:_:_:_:_:_:_:_:_:_:_:_:_: -- server-pixel-format
	 l1:l2:l3:l4:
	 _) <- recvInts sock 24

	let framebuffer = Box { x = 0
						  , y = 0
						  , w = bytesToInt [w1, w2]
						  , h = bytesToInt [h1, h2] }

	-- Get ServerName
	serverName <- recvString sock (bytesToInt [l1, l2, l3, l4])

	setEncodings sock format
	setPixelFormat sock format

	framebufferUpdateRequest sock 0 framebuffer

	display <- openDisplay ""
	rootw <- rootWindow display (defaultScreen display)
	win <- mkUnmanagedWindow display (defaultScreenOfDisplay display) rootw 100 100 ((fromIntegral (w framebuffer))) (fromIntegral (h framebuffer))
	setTextProperty display win "VNC Client" wM_NAME
	mapWindow display win
	gc <- createGC display win

	(_:_:n1:n2:_) <- recvInts sock 4

	--putStrLn "To open display window, press [Enter]..."
	--hold <- getLine

	--display screen image
	displayRectangles display win gc sock (bytesToInt [n1, n2])	
	freeGC display gc

	putStrLn "To kill application, press [Enter]..."
	hold <- getLine

	sync display False
	threadDelay (1 * 1000000)
	exitWith ExitSuccess

	-- Close socket
	sClose sock
