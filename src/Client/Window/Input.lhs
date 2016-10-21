\section{Client.Window.Input}

> module Client.Window.Input (inputHandler) where

> import Client.Network
> import Client.Types
> import Control.Concurrent (threadDelay)
> import Control.Monad.Reader
> import Graphics.X11.Xlib
> import Graphics.X11.Xlib.Extras
> import Network.Socket (Socket)

> data InputEvent = KeyEv Bool KeySym | Other

> eventList :: VNCDisplayWindow -> [IO InputEvent]
> eventList xWindow = repeat $ singleEvent
>   where
>     singleEvent :: IO InputEvent
>     singleEvent = do
>         p <- pending (display xWindow)
>         if p /= 0
>            then allocaXEvent f
>            else threadDelay 100000 >> singleEvent
>     f e = do
>         nextEvent (display xWindow) e
>         ev     <- getEvent e
>         evType <- get_EventType e
>         getEventData e ev evType
>     getEventData :: XEventPtr -> Event -> EventType -> IO InputEvent
>     getEventData ePtr e eType | eType == keyPress   = lookupKeysym (asKeyEvent ePtr) 0 >>= \k -> return $ KeyEv True  k
>                               | eType == keyRelease = lookupKeysym (asKeyEvent ePtr) 0 >>= \k -> return $ KeyEv False k
>     getEventData _ _ _ = return Other

> inputHandler :: VNCClient ()
> inputHandler = do
>     env <- ask
>     let events = eventList (xWindow env)
>     liftIO . sequence_ . fmap (f (sock env)) $ events
>   where
>     f s e = do ev <- e
>                handleEvent s ev

> handleEvent :: Socket -> InputEvent -> IO ()
> handleEvent sock (KeyEv keyPos keySym) = sendKeyEvent sock keyPos (fromIntegral keySym)
> handleEvent _     _                    = return ()

\subsection{Sending Input to Server}

> sendKeyEvent :: Socket -> Bool -> Int -> IO ()
> sendKeyEvent sock keyPos key = let downFlag = if keyPos then 1 else 0
>                                in sendInts sock ( 4 -- message type
>                                                  :downFlag
>                                                  :0:0
>                                                  :intToBytes 4 key)
>                                   >> return ()
