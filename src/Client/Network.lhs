\section {Client.Network}

> module Client.Network (
>       RFBInt(..)
>
>     , connect
>     , disconnect
>
>     , sendString
>     , recvString
>
>     , sendInt
>     , sendInts
>     , padding
>     , packIntList
>     , recvInt
>     , recvInts
>     , recvPadding
>
>     , recvFixedLengthByte
>     , recvFixedLengthChar
>
>     ) where

> import Client.Types
> import Control.Monad (replicateM,replicateM_)
> import Control.Monad.Trans.Reader (ask)
> import Data.Binary (Binary, Get, Put, get, put)
> import Data.Binary.Get (runGet)
> import Data.Binary.Put (runPut)
> import qualified Data.ByteString.Char8 as C8 (ByteString, append, length,
>                                               null, pack, unpack)
> import qualified Data.ByteString.Lazy  as B8 (ByteString, append, length,
>                                               null)
> import Data.Int (Int64)
> import qualified Network.Socket                 as Network
> import qualified Network.Socket.ByteString      as NetChar (send, recv)
> import qualified Network.Socket.ByteString.Lazy as NetByte (send, recv)

\textbf{Notes}: The network send functions currently discard the result. We
could use the result to ensure the data was sent reliably.

\subsection {Network management}

> connect :: String -> Int -> IO Network.Socket
> connect host port = do
>     serverAddr:_ <- Network.getAddrInfo Nothing (Just host) (Just $ show port)
>     sock <- Network.socket (Network.addrFamily serverAddr) Network.Stream Network.defaultProtocol
>     Network.connect sock (Network.addrAddress serverAddr)
>     return sock

> disconnect :: RFB ()
> disconnect = ask >>= liftIO . Network.close >>= return

\subsection {String network functions}

> sendString :: String -> RFB ()
> sendString str = do s <- ask
>                     liftIO . NetChar.send s $ C8.pack str
>                     return ()

> recvString :: U32 -> RFB [Char]
> recvString n = fmap C8.unpack . recvFixedLengthChar $ fromIntegral n

\subsection {Integer network functions}

> sendInt :: RFBInt a =>  a -> RFB ()
> sendInt a = do s <- ask
>                liftIO . NetByte.send s . runPut $ packInts a
>                return ()

> sendInts ::  Put -> RFB ()
> sendInts m = do s <- ask
>                 liftIO . NetByte.send s $ runPut m
>                 return ()

> padding :: Int -> Put
> padding n = replicateM_ n $ put (0 :: U8)

> packIntList :: RFBInt a => [a] -> Put
> packIntList = sequence_ . fmap packInts

> recvInt :: RFBInt a => RFB a
> recvInt = unpackInt (\f b -> recvFixedLengthByte b >>= return . runGet f)

> recvInts :: RFBInt a => Int -> RFB [a]
> recvInts n = unpackInt (\f b -> recvFixedLengthByte ((fromIntegral n)*b) >>= return . runGet (replicateM n f))

> recvPadding :: Int64 -> RFB ()
> recvPadding n = recvFixedLengthByte n >> return ()

\subsection {Low level network functions}

> recvFixedLengthByte :: Int64 -> RFB B8.ByteString
> recvFixedLengthByte l = do
>     s <- ask
>     x <- liftIO $ NetByte.recv s l
>     verifyLength x $ B8.length x
>   where
>     verifyLength x len
>       | l == len  = return x
>       | B8.null x = error "Connection Lost"
>       | otherwise = do y <- recvFixedLengthByte $ l - len
>                        return $ B8.append x y

> recvFixedLengthChar :: Int -> RFB C8.ByteString
> recvFixedLengthChar l = do
>     s <- ask
>     x <- liftIO $ NetChar.recv s l
>     verifyLength x $ C8.length x
>   where
>     verifyLength x len
>       | l == len  = return x
>       | C8.null x = error "Connection Lost"
>       | otherwise = do y <- recvFixedLengthChar $ l - len
>                        return $ C8.append x y

\subsection {RFBInt Typeclass}

> class Binary a => RFBInt a where
>     packInts :: a -> Put
>     packInts a = put a
>
>     (<+>) :: RFBInt a =>  Put -> a -> Put
>     (<+>) m a = m >> (packInts a)
>
>     unpackInt :: (Get a -> Int64 -> b) -> b

> instance RFBInt S8 where
>     unpackInt f = f get 1
> instance RFBInt S16 where
>     unpackInt f = f get 2
> instance RFBInt S32 where
>     unpackInt f = f get 4

> instance RFBInt U8 where
>     unpackInt f = f get 1
> instance RFBInt U16 where
>     unpackInt f = f get 2
> instance RFBInt U32 where
>     unpackInt f = f get 4
