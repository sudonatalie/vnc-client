\section{Client.Messages}

> module Client.Messages (
>       setPixelFormat
>     , setEncodings
>     , framebufferUpdateRequest
>     , messageKeyEvent
>     ) where

> import Client.Network
> import Client.Types
> import Control.Monad.Trans.Reader (ask)

\subsection{Client to Server Messages}

> setPixelFormat :: RFBFormat -> RFB ()
> setPixelFormat format =
>     sendInts $ packInts (0 :: U8)
>                      >> padding 3
>                     <+> bitsPerPixel format
>                     <+> depth format
>                     <+> bigEndianFlag format
>                     <+> trueColourFlag format
>                     <+> redMax format
>                     <+> greenMax format
>                     <+> blueMax format
>                     <+> redShift format
>                     <+> greenShift format
>                     <+> blueShift format
>                      >> padding 3

> setEncodings :: RFBFormat -> RFB ()
> setEncodings format =
>     sendInts $ packInts (2 :: U8)
>                      >> padding 1
>                     <+> ((fromIntegral . length $ encodingTypes format) :: U16)
>                      >> (packIntList $ encodingTypes format)

> framebufferUpdateRequest :: U8 -> VNCClient ()
> framebufferUpdateRequest incremental = do
>     fb <- framebuffer <$> ask
>     runRFB $ sendInts $ packInts (3 :: U8)
>                              <+> incremental
>                              <+> x fb
>                              <+> y fb
>                              <+> w fb
>                              <+> h fb

> messageKeyEvent :: Bool -> U32 -> RFB ()
> messageKeyEvent keyPos key = let downFlag = if keyPos then 1 else 0
>                           in sendInts $ packInts (4        :: U8)
>                                              <+> (downFlag :: U8)
>                                              <+> (0        :: U16)
>                                              <+> key

> messagePointerEvent = undefined

> clientCutText = undefined
