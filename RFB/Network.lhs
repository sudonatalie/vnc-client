\section {Network.lhs}

> module RFB.Network (bytesToInt, intToBytes, recvInts, recvString, sendInts,
>                     sendString) where

> import Network.Socket hiding (send, recv)
> import Network.Socket.ByteString (send, recv)
> import qualified Data.ByteString.Char8 as B8 (ByteString, append, length,
>                                               pack, unpack)
> import Data.Bits ((.|.), (.&.), shiftL, shiftR)
> import Data.Char (ord, chr)
> import Data.List (foldl1')

\subsection {Network Functions}

> recvString :: Socket -> Int -> IO [Char]
> recvString s l = fmap B8.unpack (recvFixedLength s l)

> recvInts :: Socket -> Int -> IO [Int]
> recvInts s l = fmap bytestringToInts (recvFixedLength s l)

> sendString :: Socket -> String -> IO Int
> sendString s l = send s (B8.pack l)

> sendInts :: Socket -> [Int] -> IO Int
> sendInts s l = send s (intsToBytestring l)

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

\subsection {Type Conversion Functions}

> bytestringToInts :: B8.ByteString -> [Int]
> bytestringToInts = map ord . B8.unpack

> intsToBytestring :: [Int] -> B8.ByteString
> intsToBytestring = B8.pack . map chr

> bytesToInt :: [Int] -> Int
> bytesToInt []  = 0
> bytesToInt [b] = b 
> bytesToInt bs  = foldl1' (\ a b -> shiftL a 8 .|. b) bs

> intToBytes :: Int -> Int -> [Int]
> intToBytes l x = let lsr = \b -> shiftR (b .&. 0xFFFFFFFFFFFFFF00) 8
>                  in reverse . take l . fmap (.&. 0xFF) $ iterate lsr x
