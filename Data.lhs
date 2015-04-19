\section{Data.lhs}

> module Data where

> data Options =  Options
>                 { optHelp :: Bool
>                 , optVerbose :: Bool
>                 , optGraphical :: Bool
>                 , optPort :: Int
>                 , optTop :: Int
>                 , optLeft :: Int
>                 , optWidth :: Maybe Int
>                 , optHeight :: Maybe Int
>                 } deriving Show
