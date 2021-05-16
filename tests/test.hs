-- |
module Test where

import Jada.Journey hiding (main)
import Lens.Micro ((^.))
import Test.Tasty (TestTree, defaultIngredients, defaultMain, defaultMainWithIngredients, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
-- import Test.Tasty.Runners.Html (htmlRunner)
import Text.Megaparsec (errorBundlePretty, parse)
import Web.Tweet (TweetEntity, text)

testParse :: TweetEntity -> TestTree
testParse entity =
  let tweet :: String
      tweet = entity ^. text
   in testCase tweet $
        case parse pTweet "tweet" tweet of
          Left error -> assertFailure ("could not parse tweet: " ++ errorBundlePretty error)
          Right t -> pure ()

main = do
  tweets <- loadTweets "tweet.store"
  -- defaultMainWithIngredients (htmlRunner : defaultIngredients) $
  defaultMain $
    testGroup "parsing stored tweets" (testParse <$> tweets)
