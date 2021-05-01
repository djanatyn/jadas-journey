-- |
module Test where

import Jada.Journey hiding (main)
import Test.Tasty
import Test.Tasty.HUnit
import Web.Tweet.Utils (displayTimelineColor)

main =
  defaultMain $
    testGroup
      "tests"
      [ testCase "load tweets" $ do
          tweets <- loadTweets "tweet.store"
          putStrLn . displayTimelineColor $ tweets
      ]
