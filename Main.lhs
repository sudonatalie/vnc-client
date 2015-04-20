\section{Main.lhs}

> import System.Environment (getArgs)
> import System.Console.GetOpt
> import Graphics.UI.Gtk
> import Graphics.UI.Gtk.Glade
> import Data
> import RFB.GUI as GUI
> import RFB.CLI as CLI

\subsection{Command Line Options}

The command line options currently supported are:
\begin{enumerate}
\item help
\item verbose
\item graphical user interface
\item authentication type (None or VNC)
\item port number
\item top, left, width and height of frame
\end{enumerate}

\subsubsection{Defaults}

Each option has a default value.

> defaultOptions =  Options
>                   { optHelp = False
>                   , optVerbose = False
>                   , optGraphical = False
>                   , optNoAuth = False
>                   , optPort = 5900
>                   , optTop = 0
>                   , optLeft = 0
>                   , optWidth = Nothing
>                   , optHeight = Nothing
>                   }

\subsubsection{Option Descriptions}

The following option descriptions are used for \texttt{--help}.

> header = "Usage: vnc-client [OPTION...] host"

> options :: [OptDescr (Options -> Options)]
> options =  [ Option ['?'] ["help"]
>                (NoArg (\ opts -> opts { optHelp = True }))
>                "print usage instructions"
>            , Option ['v'] ["verbose"]
>                (NoArg (\ opts -> opts { optVerbose = True }))
>                "verbose mode for more information output"
>            , Option ['g'] ["gui"]
>                (NoArg (\ opts -> opts { optGraphical = True }))
>                "configure client via graphical UI"
>            , Option ['n'] ["no-auth"]
>                (NoArg (\ opts -> opts { optNoAuth = True }))
>                "connect without authentication (default: VNC password authentication)"
>            , Option ['p'] ["port"]
>                (ReqArg (\ p opts -> opts { optPort = read p :: Int }) "PORT")
>                "port number (default: 5900)"
>            , Option ['t'] ["top"]
>                (ReqArg (\ t opts -> opts { optTop = read t :: Int }) "TOP")
>                "top position (default: 0)"
>            , Option ['l'] ["left"]
>                (ReqArg (\ l opts -> opts { optLeft = read l :: Int }) "LEFT")
>                "left position (default: 0)"
>            , Option ['w'] ["width"]
>                (ReqArg (\ w opts -> opts { optWidth = Just (read w :: Int) }) "WIDTH")
>                "width (default: entire framebuffer)"
>            , Option ['h'] ["height"]
>                (ReqArg (\ h opts -> opts { optHeight = Just (read h :: Int) }) "HEIGHT")
>                "height (default: entire framebuffer)"
>            ]

> parseOpts :: [String] -> IO (Options, [String])
> parseOpts argv =
>     case getOpt Permute options argv of
>         (o,n,[]  )  -> return (foldl (flip id) defaultOptions o, n)
>         (_,_,errs)  -> ioError (userError (concat errs ++ usageInfo header options))

\subsection{Main function}

> main = do

\subsubsection{Arguments}

First, get and parse the command line arguments as options.

>     args <- getArgs
>     (opts@Options  { optHelp       = help
>                    , optVerbose    = verbose
>                    , optGraphical  = gui
>                    , optNoAuth     = noAuth
>                    , optPort       = port
>                    , optTop        = top
>                    , optLeft       = left
>                    , optWidth      = width
>                    , optHeight     = height }
>         , params) <- parseOpts args

\subsubsection{Help}

>     if (help)
>         then putStrLn (usageInfo header options)
>     else do

If the help option is specified, then the following descriptive message is displayed:

\begin{verbatim}
~ $ vnc-client --help
Usage: vnc-client [OPTION...] host
  -?         --help           print usage instructions
  -v         --verbose        verbose mode for more information output
  -g         --gui            configure client via graphical UI
  -n         --no-auth        connect without authentication (default: 
VNC password authentication)
  -p PORT    --port=PORT      port number (default: 5900)
  -t TOP     --top=TOP        top position (default: 0)
  -l LEFT    --left=LEFT      left position (default: 0)
  -w WIDTH   --width=WIDTH    width (default: entire framebuffer)
  -h HEIGHT  --height=HEIGHT  height (default: entire framebuffer)
\end{verbatim}

\subsubsection{GUI}

Launch the GUI if it is specifically requested, or if the hostname is unspecified.

>         if (gui || null params)
>             then do
>                 initGUI
>                 Just xml <- xmlNew "gui.glade"
>                 window <- xmlGetWidget xml castToWindow "window"
>                 onDestroy window mainQuit
>                 closeButton <- xmlGetWidget xml castToButton "disconnectButton"
>                 onClicked closeButton $ do
>                     widgetDestroy window
>                 entry <- xmlGetWidget xml castToEntry "entry"
>                 passwordBox <- xmlGetWidget xml castToEntry "password"
>                 connectButton <- xmlGetWidget xml castToButton "connectButton"
>                 onClicked connectButton $ do
>                     host <- get entry entryText
>                     password <- get passwordBox entryText
>                     GUI.connect host opts password
>                 widgetShowAll window
>                 mainGUI

\subsubsection{CLI}

Launch the CLI otherwise.

>         else do
>             case params of
>                 [host]  -> CLI.connect host opts
>                 _       -> ioError (userError ("too many arguments\n" ++ usageInfo header options))
