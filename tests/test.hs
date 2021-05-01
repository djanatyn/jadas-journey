-- |
module Test where

import qualified Jada.Journey hiding (main)
import Test.Tasty
import Test.Tasty.HUnit

main = defaultMain $
  testCase "first test" $ do
    2 + 2 @?= 4
