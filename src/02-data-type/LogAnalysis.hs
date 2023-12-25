{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage msg =
    let
        logComponent = words msg
        toInt a = read a :: Int
        parseError parts = case parts of
            (errCode : severityCode : msgs) -> Log.LogMessage (Log.Error $ toInt errCode) (toInt severityCode) (unwords msgs)
            _ -> Log.Unknown "Invalid error message form"
     in
        case logComponent of
            ("I" : severityCode : xs) -> Log.LogMessage Log.Info (toInt severityCode) (unwords xs)
            ("W" : severityCode : xs) -> Log.LogMessage Log.Warning (toInt severityCode) (unwords xs)
            ("E" : xs) -> parseError xs
            _ -> Log.Unknown "This is not in the right format"

parse :: String -> [LogMessage]
parse inputLogs = map parseMessage $ lines inputLogs
