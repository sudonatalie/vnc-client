\section{Data.lhs}

> module Data where

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
