{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}

module Jada.Journey
  ( -- Types
    Reward (..),
    Kill (..),
    JadaTweet (..),
    -- Serialization
    storeTweets,
    loadTweets,
    -- Fetch Timeline
    jadaRPGTimeline,
    -- Parsing
    pTweet,
  )
where

import qualified Data.ByteString as BS
import Data.Store (Store, decodeEx, encode)
import Data.Void (Void)
import GHC.Generics (Generic)
import Lens.Micro (each, (^.), (^..))
import Text.Megaparsec
import Text.Megaparsec.Char (alphaNumChar, char, numberChar, printChar, space, string)
import qualified Text.Megaparsec.Char.Lexer as L (lexeme)
import Web.Tweet (Timeline, TweetEntity (..), getAll)
import Web.Tweet.Utils (displayTimelineColor)

-- allow serialization of tweets
deriving instance Generic TweetEntity

instance Store TweetEntity

type Parser = Parsec Void String

data Reward where
  Level :: String -> Reward
  Item :: String -> Reward
  deriving (Show, Generic, Store)

data Kill where
  Kill :: {style :: String, target :: String, reward :: Reward} -> Kill
  deriving (Show, Generic, Store)

data JadaTweet where
  Discovery :: String -> JadaTweet
  Train :: String -> JadaTweet
  Enemy :: Kill -> JadaTweet
  Flavor :: String -> JadaTweet
  deriving (Show, Generic, Store)

lexeme = L.lexeme space

pLevel :: Parser Reward
pLevel = do
  string "and reached level "
  level <- some numberChar
  return $ Level level

pItem :: Parser Reward
pItem = do
  string "found a "
  item <- someTill printChar (char '!')
  return $ Item item

pKill :: Parser JadaTweet
pKill = do
  string "Jada "
  style <- lexeme $ someTill printChar (string "killed a")
  target <- lexeme $ some (alphaNumChar <|> char '-' <|> char '”')
  reward <- try pLevel <|> try pItem
  return $ Enemy $ Kill {style, target, reward}

pDiscovery :: Parser JadaTweet
pDiscovery = do
  choice $
    try . lexeme . string
      <$> [ "Jada has discovered the",
            "Jada stumbled across the",
            "Jada notices a group of people headed west. He asks where they are going. A young orphan among them notices that this man is the Beetle King himself! The orphan explains that they are headed for the",
            "Jada trips into a deep hole, at the bottom he falls into a portal that transported him to the",
            "Jada notices the clouds shifting and it's suddenly chilly. He adjusts the straps of bag before leaving"
          ]
  discovery <- lexeme $ someTill printChar (char '.')
  choice $
    try . lexeme . string
      <$> [ "The monsters here are unforgiving but the Beetle King will persevere.  He makes camp for the night as he prepares to leave the",
            "Its very chilly but the Beetle King has endured worse.  Despite the inclimate weather he is ready to tackle whatever lies ahead.  He takes only what he needs as he leaves the",
            "Jada decides to follow them and leave the",
            "This would've been unexpected for anyone but the Beetle King seems unphased. I guess it wasn't meant for him to be in the",
            "Following a well-worn path, Jada marches forth for hours, eventually coming across"
          ]
  oldPlace <- lexeme $ someTill printChar (char '.')
  skipMany $ try $ string "any longer..."
  return $ Discovery discovery

pTrain :: Parser JadaTweet
pTrain = do
  lexeme . string $ "Jada notices a train headed for the"
  destination <- lexeme $ someTill printChar (char '.')
  lexeme . string $ "The Beetle King jumps on the train and is whisked away from the"
  oldPlace <- lexeme $ someTill printChar (char '.')
  lexeme . string $ "The adventure must go on!"
  return $ Train destination

pFlavor :: Parser JadaTweet
pFlavor = do
  text <-
    choice $
      try . string
        <$> [ "Building enemies...",
              "Entering the world of... Jada MMORPG",
              "Jada has entered the world at level 1!",
              "The finest finery for supporters of the Beetle King and his adventures. https://t.co/xJuUWWmnpY",
              "Jada's bard finally learned how to sing.",
              "Jada looks up to the sky and sees a carrier pidgeon with a note.  The pidgeon drops the note and the parchment elegantly descends to the ground. Jada picks it up and looks at it. The note reads: \"This is a test.\"",
              "Jada gets a call from his boss telling him to meet at the Wasteland of Could.  He quickly packs his things and leaves Dungeon of Because.  Upon arriving the Beetle King notices nobody is there and he yells to the sky: \"Those darned prank callers!\" while shaking his fist."
            ]
  return $ Flavor text

pScribe :: Parser JadaTweet
pScribe = do
  text <-
    choice $
      try . string
        <$> [ "The man says he is a travelling bard looking for an inspirational adventurer to follow and use for his poetry.   Jada agrees since he needs a new scribe.  The man gracefully hops down from his rock, but not before playing a single note on his lute: a C#. \n\n\"Onward then!\"",
              "As the sun crests the eastern horizon Jada awakens to find his old scribe gone.  A strange man sits on a rock nearby playing a lute.",
              "Jada's scribe is acting strange.  They go to sleep for the night in hopes that the next day will be an adventurous one."
            ]
  return $ Flavor text

pResponse :: Parser JadaTweet
pResponse = do
  text <-
    choice $
      try . string
        <$> [ "@jadachoa You're already level 13 Beetle King sir.",
              "@jadachoa Now thats some high level gear!",
              "@djanatyn Bots that track the progress of bots...  This is too far!!!",
              "To be honest I'm just the humble scribe of the Beetle King."
            ]
  return $ Flavor text

pTweet :: Parser JadaTweet
pTweet =
  try pFlavor
    <|> try pScribe
    <|> try pResponse
    <|> try pTrain
    <|> try pKill
    <|> try pDiscovery

jadaRPGTimeline :: FilePath -> IO Timeline
jadaRPGTimeline = getAll "jadaRPG" Nothing

storeTweets :: FilePath -> Timeline -> IO ()
storeTweets path timeline = BS.writeFile path $ encode timeline

loadTweets :: FilePath -> IO Timeline
loadTweets path = decodeEx <$> BS.readFile path

main :: IO ()
main = do
  tweets <- loadTweets "tweet.store"
  putStrLn . displayTimelineColor $ tweets
