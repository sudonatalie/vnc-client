\section{Client.Handshake}

> module Client.Handshake (
>       negotiateProtocolVersion
>     , negotiateSecurity
>     ) where

> import Client.Network
> import Client.Security (hashVNCPassword)
> import Client.Types
> import Control.Exception (bracket_)
> import System.IO (hGetEcho, hFlush, hSetEcho, stdin, stdout) 

\subsection{Negotiating RFB Protocol Version}

> negotiateProtocolVersion :: [RFBVersion] -> VNCClient ()
> negotiateProtocolVersion _ = do
>     msg <- runRFB $ recvString 12
>     status $ "Server Protocol Version: " ++ msg
>     let version = "RFB 003.007\n"
>     status $ "Requsted Protocol Version: " ++ version
>     runRFB $ sendString version

\subsection{Negotiating Security and Authenticating User}

> negotiateSecurity :: SecurityType -> Maybe String -> VNCClient ()
> negotiateSecurity securityType mPwd = do
>     numSecurityTypes :: U8   <- runRFB recvInt
>     serverSecurityTypes    :: [U8] <- runRFB $ recvInts (fromIntegral numSecurityTypes)
>     status $ "Server Security Types: " ++ show serverSecurityTypes
>     authenticateUser securityType mPwd
>     return ()

> authenticateUser :: SecurityType -> Maybe String -> VNCClient U32
> authenticateUser secType Nothing = do 
>     interface <- ui <$> getClientInfo
>     case interface of
>       CLI -> do password <- liftIO getPassword
>                 result <- authenticateUser' secType password
>                 case result of
>                   0 -> return 0
>                   1 -> liftIO (putStrLn "Invalid Password.") >> authenticateUser secType Nothing
>                   2 -> error "Authenication failed: too many password attempts."
>                   _ -> error "Malformed security result."
>       GUI -> error "no password supplied"
> authenticateUser secType (Just password) = authenticateUser' secType password

> authenticateUser' :: SecurityType -> String -> VNCClient U32
> authenticateUser' NoAuth _ = runRFB $ sendInt (1 :: U8) >> return 0
> authenticateUser' VNCAuth password = do
>     runRFB $ sendInt (2 :: U8)
>     challenge :: [U8] <- runRFB $ recvInts 16
>     runRFB $ sendInts . packIntList $ hashVNCPassword password challenge
>     securityResult :: U32 <- runRFB recvInt
>     return securityResult

> getPassword :: IO String
> getPassword = do
>     putStr "Input Password: "
>     hFlush stdout
>     pass <- withEcho False getLine
>     putChar '\n'
>     return pass
>   where
>     withEcho :: Bool -> IO a -> IO a
>     withEcho echo action = do
>       old <- hGetEcho stdin
>       bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action
