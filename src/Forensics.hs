module Forensics where

import Data.Maybe (mapMaybe)
import Prelude hiding (log)
import Text.Read (readMaybe)

-- Warmup: Let's remember how to write some standard higher-order functions on lists.

-- Reimplement Prelude.map
-- Apply the function to every element of the list.
-- myMap (+ 1) [1, 2, 3] == [2, 3, 4]
myMap :: (a -> b) -> [a] -> [b]
myMap f xs = error "TODO"

-- Reimplement Prelude.filter
-- Return the elements of the list that satisfy the predicate.
-- myFilter (\x -> x > 0) [-1, 0, 1] == [1]
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f xs = error "TODO"

-- Remember the definition of Maybe:
-- data Maybe a = Just a | Nothing

-- Reimplement Data.Maybe.mapMaybe
-- Apply the function and keep the values inside 'Just', throwing away 'Nothing's.
-- myMapMaybe (\x -> if x > 0 then Just (x + 1) else Nothing) [-1, 0, 1] == [2]
myMapMaybe :: (a -> Maybe b) -> [a] -> [b]
myMapMaybe f xs = error "TODO"

-- Now the fun stuff:
-- You just got paged because a server went down at work. Your job is to
-- find out what happened by looking at logs.

-- A simple timestamp for log entries.
type Time = Int

-- Every entry in the log is a pair of (timestamp, some message)
data LogEntry a = LogEntry Time a
  deriving (Eq, Show)

-- Our initial log entries are just textual (timestamp, string message)
type TextLogEntry = LogEntry String

-- Our initial log is a list of textual entries
type TextLog = [TextLogEntry]

-- Here's a log we've been able to gather:
-- It consists of server status entries and client connection entries.
hackLog :: TextLog
hackLog =
  [ LogEntry 10 "server status ok"
  , LogEntry 20 "client 1 connected"
  , LogEntry 30 "client 2 connected"
  , LogEntry 40 "server status ok"
  , LogEntry 50 "client 3 connected"
  , LogEntry 60 "client 1 connected"
  , LogEntry 70 "server status error"
  ]

-- Our server status is either ok or error
data Status = StatusOk | StatusError
  deriving (Eq, Show)

-- We can parse the textual form of the log into a nicer message
data ParsedMessage =
    ServerStatus Status
  | ClientConnected Int
  deriving (Eq, Show)

-- This does the parsing
parseLogMessage :: String -> Maybe ParsedMessage
parseLogMessage s =
  case words s of
    ["server", "status", x] ->
      let status = if x == "ok" then StatusOk else StatusError
      in Just (ServerStatus status)
    ["client", x, "connected"] ->
      case readMaybe x of
        Just i -> Just (ClientConnected i)
        _ -> Nothing
    _ -> Nothing

-- After parsing we have log entries of the form (timestamp, parsed message)
type ParsedLogEntry = LogEntry ParsedMessage

-- Transforms a textual log entry into a parsed one
parseLogEntry :: TextLogEntry -> Maybe ParsedLogEntry
parseLogEntry entry = error "TODO"

-- Our parsed log is a list of parsed log entries!
type ParsedLog = [ParsedLogEntry]

-- Transforms a textual log into a parsed one
-- Can you do it with some list functions we know?
-- How about point-free (using `.`)?
parseLog :: TextLog -> ParsedLog
parseLog log = error "TODO"

-- Extracts all client ids from the parsed log
-- Can you do it with some list functions we know?
-- How about point-free (using `.`)?
allClients :: ParsedLog -> [Int]
allClients plog = error "TODO"

-- Can you find all clients who connected between server ok and server error?
-- Maybe we think we can trust clients that connected before and left the server ok.
-- How about all unique clients in that time?

-- Now we have gathered more logs and have a hypothesis:
-- Maybe the error has something to do with server maintenance!
-- We've managed to add maintenance log entries of the form
-- tech [NAME] powered [on|off] server

-- For example:
expandedLog :: TextLog
expandedLog =
  [ LogEntry 10 "server status ok"
  , LogEntry 20 "client 1 connected"
  , LogEntry 21 "tech bob powered off server"
  , LogEntry 23 "tech bob powered on server"
  , LogEntry 30 "client 2 connected"
  , LogEntry 40 "server status ok"
  , LogEntry 50 "client 3 connected"
  , LogEntry 60 "client 1 connected"
  , LogEntry 64 "tech alice powered off server"
  , LogEntry 70 "server status error"
  ]

-- Can you find the name of all techs working on the server?
-- What about the last tech to power off the server?

-- We can define a common interface for these parsing operations:
class LogTransformer p where
  transformLogMessage :: String -> Maybe p

  -- We can provide a default implementation for this!
  transformLogEntry :: LogEntry String -> Maybe (LogEntry p)
  transformLogEntry entry = error "TODO"

  -- We can provide a default implementation for this too!
  transformLog :: [LogEntry String] -> [LogEntry p]
  transformLog log = error "TODO"

-- The interface matches up with the operations we've defined on 'ParsedMessage'
instance LogTransformer ParsedMessage where
  transformLogMessage = parseLogMessage

-- Can you provide an implementation that includes tech information?
