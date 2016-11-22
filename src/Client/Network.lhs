\section {Client.Network}

> module Client.Network (bytesToInt, intToBytes, recvInts, recvString, sendInts,
>                     sendString) where

> import Data.Bits ((.|.), (.&.), shiftL, shiftR)
> import qualified Data.ByteString.Char8 as C8 (ByteString, append, length,
>                                               null, pack, unpack)
> import qualified Data.ByteString.Lazy  as B8 (ByteString, append, length,
>                                               null, pack, unpack)
> import Data.Char (ord, chr)
> import Data.Int (Int64)
> import Data.List (foldl1')
> import Network.Socket (Socket)
> import qualified Network.Socket.ByteString      as NetChar (send, recv)
> import qualified Network.Socket.ByteString.Lazy as NetByte (send, recv)

\subsection {Network Functions}

> recvString :: Socket -> Int -> IO [Char]
> recvString s l = fmap C8.unpack $ recvFixedLengthChar s l

> recvInts :: Socket -> Int -> IO [Int]
> recvInts s l = fmap bytestringToInts $ recvFixedLengthByte s (fromIntegral l)

> sendString :: Socket -> String -> IO Int
> sendString s str = NetChar.send s $ C8.pack str

> sendInts :: Socket -> [Int] -> IO Int64
> sendInts s xs = NetByte.send s $ intsToBytestring xs

> recvFixedLengthByte :: Socket -> Int64 -> IO B8.ByteString
> recvFixedLengthByte s l = do
>     x <- NetByte.recv s l
>     let len = B8.length x
>     if len < l
>       then if B8.null x
>              then error "Connection Lost"
>              else do
>                y <- recvFixedLengthByte s (l - len)
>                return $ B8.append x y
>       else return x

> recvFixedLengthChar :: Socket -> Int -> IO C8.ByteString
> recvFixedLengthChar s l = do
>     x <- NetChar.recv s l
>     let len = C8.length x
>     if len < l
>       then if C8.null x
>              then error "Connection Lost"
>              else do
>                y <- recvFixedLengthChar s (l - len)
>                return $ C8.append x y
>       else return x

\subsection {Type Conversion Functions}

> bytestringToInts :: B8.ByteString -> [Int]
> bytestringToInts = map fromIntegral . B8.unpack

> intsToBytestring :: [Int] -> B8.ByteString
> intsToBytestring = B8.pack . map fromIntegral

> bytesToInt :: [Int] -> Int
> bytesToInt []  = 0
> bytesToInt [b] = b 
> bytesToInt bs  = foldl1' (\ a b -> shiftL a 8 .|. b) bs

> intToBytes :: Int -> Int -> [Int]
> intToBytes l x = let lsr = \b -> shiftR (b .&. 0xFFFFFFFFFFFFFF00) 8
>                  in reverse . take l . fmap (.&. 0xFF) $ iterate lsr x
