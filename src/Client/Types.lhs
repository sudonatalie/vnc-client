\section{Client.Types}

> module Client.Types
>    ( module Client.Types
>    , module Control.Monad.IO.Class -- export MonadIO typeclass and liftIO
>    ) where

> import Control.Monad.IO.Class
> import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
> import Data.Int (Int8, Int16, Int32)
> import Data.Word (Word8, Word16, Word32)
> import Graphics.X11.Xlib (Display, Dimension, Drawable, GC, Pixmap, Window)
> import Network.Socket (Socket)

> data Options =  Options
>                 { optHelp      :: Bool
>                 , optVerbose   :: Bool
>                 , optGraphical :: Bool
>                 , optNoAuth    :: Bool
>                 , optPort      :: Int
>                 , optTop       :: U16
>                 , optLeft      :: U16
>                 , optWidth     :: Maybe U16
>                 , optHeight    :: Maybe U16
>                 , optBPP       :: U8
>                 } deriving Show

> data RFBFormat =  RFBFormat
>                   { encodingTypes   :: [S32]
>                   , bitsPerPixel    :: U8
>                   , depth           :: U8
>                   , bigEndianFlag   :: U8
>                   , trueColourFlag  :: U8
>                   , redMax          :: U16
>                   , greenMax        :: U16
>                   , blueMax         :: U16
>                   , redShift        :: U8
>                   , greenShift      :: U8
>                   , blueShift       :: U8
>                   } deriving (Show)

\subsection{RFB Network Types}

> type RFB = ReaderT Socket IO

> class MonadIO m => RFBNetwork m where
>     runRFB :: RFB a -> m a

> -- useful for writing typclass instances
> runRFBWithSocket :: MonadIO m => Socket -> RFB a -> m a
> runRFBWithSocket s m = liftIO $ runReaderT m s

The following type renames are used to keep the Int type names consistent with
those used in the RFB spec.

> type S8  = Int8
> type S16 = Int16
> type S32 = Int32

> type U8  = Word8
> type U16 = Word16
> type U32 = Word32

\subsection{Client Window Types}

> type VNCClient = ReaderT Environment IO

> instance RFBNetwork VNCClient where
>     runRFB m = do env <- ask
>                   runRFBWithSocket (sock env) m

> data Environment = Environment
>                    { sock        :: Socket
>                    , framebuffer :: Box
>                    , xWindow     :: VNCDisplayWindow
>                    , leftOffset  :: U16
>                    , topOffset   :: U16
>                    }

> data Box =  Box
>             { x  :: U16
>             , y  :: U16
>             , w  :: U16
>             , h  :: U16
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
>                          , bpp      :: U8
>                          }

> type Color = Drawable
