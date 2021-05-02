-- |
module Test where

import Data.Either
import Jada.Journey hiding (main)
import Lens.Micro
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Runners.Html
import Text.Megaparsec
import Web.Tweet
import Web.Tweet.Utils (displayTimelineColor)

testParse :: TweetEntity -> TestTree
testParse entity =
  let tweet :: String
      tweet = entity ^. text
   in testCase tweet $
        case parse pTweet "tweet" tweet of
          Left error -> assertFailure ("could not parse tweet: " ++ errorBundlePretty error)
          Right t -> print t

main = do
  tweets <- loadTweets "tweet.store"
  defaultMainWithIngredients (htmlRunner : defaultIngredients) $
    testGroup "parsing stored tweets" (testParse <$> tweets)
