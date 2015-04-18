\documentclass{article}
%include lhs2TeX.fmt
%include lhs2TeX.sty
\begin{document}

> import System.Environment (getArgs)
> import System.Console.GetOpt
> import Graphics.UI.Gtk
> import Graphics.UI.Gtk.Glade
> import RFB.GUI as GUI
> import RFB.CLI as CLI

Command line options

> data Options = Options
>     { optHelp :: Bool
>     , optVerbose :: Bool
>     , optGraphical :: Bool
>     , optPort :: Int
>     } deriving Show

Default options

> defaultOptions = Options
>     { optHelp = False
>     , optVerbose = False
>     , optGraphical = False
>     , optPort = 5900
>     }

> header = "Usage: vnc-client [OPTION...] host"

Option descriptions

> options :: [OptDescr (Options -> Options)]
> options =
>     [ Option ['h'] ["help"]
>              (NoArg (\ opts -> opts { optHelp = True }))
>              "print usage instructions"
>     , Option ['v'] ["verbose"]
>              (NoArg (\ opts -> opts { optVerbose = True }))
>              "verbose mode for more information output"
>     , Option ['g'] ["gui"]
>         (NoArg (\ opts -> opts { optGraphical = True }))
>         "configure client via graphical UI"
>     , Option ['p'] ["port"]
>         (ReqArg (\ p opts -> opts { optPort = read p :: Int }) "PORT")
>         "port number (default: 5900)"
>     ]

> parseOpts :: [String] -> IO (Options, [String])
> parseOpts argv =
>     case getOpt Permute options argv of
>         (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
>         (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))

> main = do
>     -- Get and parse command line arguments
>     args <- getArgs
>     (Options
>             { optHelp = help
>             , optVerbose = verbose
>             , optGraphical = gui
>             , optPort = port }
>         , params) <- parseOpts args

>     if (help)
>         then putStrLn (usageInfo header options)
>         else do
>             -- Launch GUI if requested or hostname unspecified
>             if (gui || null params)
>                 then do
>                     initGUI
>                     Just xml <- xmlNew "gui.glade"
>                     window <- xmlGetWidget xml castToWindow "window"
>                     onDestroy window mainQuit
>                     closeButton <- xmlGetWidget xml castToButton "disconnectButton"
>                     onClicked closeButton $ do
>                         widgetDestroy window
>                     entry <- xmlGetWidget xml castToEntry "entry"
>                     passwordBox <- xmlGetWidget xml castToEntry "password"
>                     connectButton <- xmlGetWidget xml castToButton "connectButton"
>                     onClicked connectButton $ do
>                         host <- get entry entryText
>                         password <- get passwordBox entryText
>                         if (verbose)
>                             then putStrLn ("Connecting to " ++ host ++ ":" ++ show port ++ "...")
>                             else return ()
>                         GUI.connect host 5900 password
>                     widgetShowAll window
>                     mainGUI
>             -- Otherwise continue with CLI
>                 else do
>                     case params of
>                         [host] -> do
>                             if (verbose)
>                                 then putStrLn ("Connecting to " ++ host ++ ":" ++ show port ++ "...")
>                                 else return ()
>                             CLI.connect host port
>                         _ -> ioError (userError ("too many arguments\n" ++ usageInfo header options))

\end{document}
