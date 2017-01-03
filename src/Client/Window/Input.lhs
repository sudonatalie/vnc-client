\section{Client.Window.Input}

> module Client.Window.Input (inputHandler) where

> import Client.Messages (messageKeyEvent)
> import Client.Types
> import Control.Concurrent (threadDelay)
> import Graphics.X11.Xlib
> import Graphics.X11.Xlib.Extras

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
>     getEventData ePtr _ eType | eType == keyPress   = lookupKeysym (asKeyEvent ePtr) 0 >>= \k -> return $ KeyEv True  k
>                               | eType == keyRelease = lookupKeysym (asKeyEvent ePtr) 0 >>= \k -> return $ KeyEv False k
>     getEventData _ _ _ = return Other
	
> inputHandler :: VNCWindow ()
> inputHandler = do
>     xWin <- xWindow <$> getWindowInfo
>     let events = eventList xWin
>     sequence_ . fmap f $ events
>   where
>     f e = do ev <- liftIO e
>              handleEvent ev

> handleEvent :: InputEvent -> VNCWindow ()
> handleEvent (KeyEv keyPos keySym) = runRFB $ messageKeyEvent keyPos (fromIntegral keySym)
> handleEvent  _                    = return ()
