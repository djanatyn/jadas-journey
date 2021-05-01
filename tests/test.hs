-- |
module Test where

import Data.Maybe
import Jada.Journey hiding (main)
import Lens.Micro
import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec
import Web.Tweet
import Web.Tweet.Utils (displayTimelineColor)

testParse :: TweetEntity -> TestTree
testParse entity =
  let tweet :: String
      tweet = entity ^. text

      parsed :: Maybe JadaTweet
      parsed = parseMaybe pTweet tweet
   in testCase "parsing tweets" $
        assertBool
          ("could not parse tweet: " ++ tweet)
          (isJust parsed)

main = do
  tweets <- loadTweets "tweet.store"
  defaultMain $
    testGroup "tests" (testParse <$> tweets)
