\section{Client.Handshake}

> module Client.Handshake (
>       negotiateProtocolVersion
>     , negotiateSecurity
>     ) where

> import Client.Network
> import Client.Security (hashVNCPassword)
> import Client.Types
> import Control.Exception (bracket_)
> import Control.Monad (when)
> import System.IO (hGetEcho, hFlush, hSetEcho, stdin, stdout) 

\subsection{Negotiating RFB Protocol Version}

> negotiateProtocolVersion :: [RFBVersion] -> VNCClient ()
> negotiateProtocolVersion _ = do
>     msg <- runRFB $ recvString 12
>     status $ "Server Protocol Version: " ++ init msg
>     let version = "RFB 003.007\n"
>     status $ "Requsted Protocol Version: " ++ init version
>     runRFB $ sendString version

\subsection{Negotiating Security and Authenticating User}

> negotiateSecurity :: U8 -> Maybe String -> VNCClient ()
> negotiateSecurity securityType mPwd = do
>     numSecurityTypes :: U8 <- runRFB recvInt
>     serverSecurityTypes :: [U8] <- runRFB $ recvInts (fromIntegral numSecurityTypes)
>     when (null serverSecurityTypes) (error "Connection failed: Server terminated connection for unknown reason. Server's number of security types was zero.")
>     status $ "Server Security Types: " ++ show serverSecurityTypes
>     when (not $ securityType `elem` serverSecurityTypes) (error "Requested client security type is not supported by the server.")
>     securityResult <- authenticateUser securityType mPwd
>     handleSecurityResult securityResult

> authenticateUser :: U8 -> Maybe String -> VNCClient U32
> authenticateUser 1 _ = authenticateUser' 1 ""
> authenticateUser secType Nothing = do 
>     interface <- ui <$> getClientInfo
>     case interface of
>       CLI -> do password <- liftIO getPassword
>                 authenticateUser' secType password
>       GUI -> error "No password supplied."
> authenticateUser secType (Just password) = authenticateUser' secType password

> authenticateUser' :: U8 -> String -> VNCClient U32
> authenticateUser' 1 _ = runRFB $ sendInt (1 :: U8) >> return 0
> authenticateUser' 2 password = do
>     runRFB $ sendInt (2 :: U8)
>     challenge :: [U8] <- runRFB $ recvInts 16
>     runRFB $ sendInts . packIntList $ hashVNCPassword password challenge
>     securityResult :: U32 <- runRFB recvInt
>     return securityResult

> handleSecurityResult :: U32 -> VNCClient ()
> handleSecurityResult 0 = return ()
> handleSecurityResult 1 = error "Authentication failed: possibly incorrect password."
> handleSecurityResult 2 = error "Authentication failed: too many password attempts."
> handleSecurityResult _ = error "Malformed security result."

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
