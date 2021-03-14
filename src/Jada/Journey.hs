module Jada.Journey
  ( main,
  )
where

import Web.Tweet
import Web.Tweet.API
import Web.Tweet.Utils

jadaRPGTimeline :: FilePath -> IO Timeline
jadaRPGTimeline = getAll "jadaRPG" Nothing

main :: IO ()
main =
  jadaRPGTimeline ".cred.toml" >>= putStrLn . displayTimeline
