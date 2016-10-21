\section{Client.Types}

> module Client.Types where

> import Control.Monad.Reader (ReaderT)
> import Graphics.X11.Xlib (Display, Dimension, GC, Pixmap, Window)
> import Network.Socket (Socket)

> data Options =  Options
>                 { optHelp :: Bool
>                 , optVerbose :: Bool
>                 , optGraphical :: Bool
>                 , optNoAuth :: Bool
>                 , optPort :: Int
>                 , optTop :: Int
>                 , optLeft :: Int
>                 , optWidth :: Maybe Int
>                 , optHeight :: Maybe Int
>                 , optBPP :: Int
>                 } deriving Show

> data RFBFormat =  RFBFormat
>                   { encodingTypes   :: [Int]
>                   , bitsPerPixel    :: Int
>                   , depth           :: Int
>                   , bigEndianFlag   :: Int
>                   , trueColourFlag  :: Int
>                   , redMax          :: Int
>                   , greenMax        :: Int
>                   , blueMax         :: Int
>                   , redShift        :: Int
>                   , greenShift      :: Int
>                   , blueShift       :: Int
>                   } deriving (Show)

\subsection{Client Window}

> type VNCClient = ReaderT Environment IO

> data Environment = Environment
>                    { sock        :: Socket
>                    , framebuffer :: Box
>                    , xWindow     :: VNCDisplayWindow
>                    , leftOffset  :: Int
>                    , topOffset   :: Int
>                    }

> data Box =  Box
>             { x  :: Int
>             , y  :: Int
>             , w  :: Int
>             , h  :: Int
>             } deriving (Show)

> data VNCDisplayWindow =  VNCDisplayWindow
>                          { display  :: Display  -- X display
>                          , rootw    :: Window   -- root window
>                          , win      :: Window   -- output window
>                          , pixmap   :: Pixmap   -- image buffer
>                          , wingc    :: GC       -- graphics contexts
>                          , pixgc    :: GC
>                          , width    :: Dimension
>                          , height   :: Dimension
>                          , bpp      :: Int
>                          }
