{-# LANGUAGE NamedFieldPuns #-}

module Jada.Journey
  ( main,
  )
where

import Data.Void
import Lens.Micro
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L (lexeme)
import Text.Megaparsec.Debug (dbg)
import Web.Tweet
import Web.Tweet.API
import Web.Tweet.Utils

type Parser = Parsec Void String

data Reward = Level String | Item String deriving (Show)

data Kill = Kill
  { killStyle :: String,
    killTarget :: String,
    killReward :: Reward
  }
  deriving (Show)

data JadaTweet
  = Discovery String
  | Train String
  | Enemy Kill
  | Flavor String
  deriving (Show)

lexeme = L.lexeme space

pLevel :: Parser Reward
pLevel = do
  string "and reached level "
  level <- some numberChar
  return $ Level level

pItem :: Parser Reward
pItem = do
  string "found a "
  item <- someTill asciiChar (char '!')
  return $ Item item

pKill :: Parser JadaTweet
pKill = dbg "kill" $ do
  string "Jada "
  killStyle <- lexeme $ some letterChar
  string "killed a "
  killTarget <- lexeme $ some alphaNumChar
  killReward <- try pLevel <|> try pItem
  return $ Enemy $ Kill {killStyle, killTarget, killReward}

pDiscovery :: Parser JadaTweet
pDiscovery = dbg "discovery" $ do
  choice $
    try . lexeme . string
      <$> [ "Jada has discovered the",
            "Jada stumbled across the",
            "Jada notices a group of people headed west. He asks where they are going. A young orphan among them notices that this man is the Beetle King himself! The orphan explains that they are headed for the",
            "Jada trips into a deep hole, at the bottom he falls into a portal that transported him to the"
          ]
  discovery <- lexeme $ someTill printChar (char '.')
  choice $
    try . lexeme . string
      <$> [ "The monsters here are unforgiving but the Beetle King will persevere.  He makes camp for the night as he prepares to leave the",
            "Its very chilly but the Beetle King has endured worse.  Despite the inclimate weather he is ready to tackle whatever lies ahead. He takes only what he needs as he leaves the",
            "Jada decides to follow them and leave the",
            "This would've been unexpected for anyone but the Beetle King seems unphased. I guess it wasn't meant for him to be in the"
          ]
  oldPlace <- lexeme $ someTill printChar (char '.')
  skipMany $ try $ string "any longer..."
  return $ Discovery discovery

pTrain :: Parser JadaTweet
pTrain = dbg "train" $ do
  lexeme . string $ "Jada notices a train headed for the"
  destination <- lexeme $ someTill printChar (char '.')
  lexeme . string $ "The Beetle King jumps on the train and is whisked away from the"
  oldPlace <- lexeme $ someTill printChar (char '.')
  lexeme . string $ "The adventure must go on!"
  return $ Train destination

pFlavor :: Parser JadaTweet
pFlavor = dbg "flavor" $ do
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
pScribe = dbg "scribe" $ do
  text <-
    choice $
      try . string
        <$> [ "The man says he is a travelling bard looking for an inspirational adventurer to follow and use for his poetry.   Jada agrees since he needs a new scribe.  The man gracefully hops down from his rock, but not before playing a single note on his lute: a C#. \n\n\"Onward then!\"",
              "As the sun crests the eastern horizon Jada awakens to find his old scribe gone.  A strange man sits on a rock nearby playing a lute.",
              "Jada's scribe is acting strange.  They go to sleep for the night in hopes that the next day will be an adventurous one."
            ]
  return $ Flavor text

pResponse :: Parser JadaTweet
pResponse = dbg "response" $ do
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

main :: IO ()
main = do
  timeline <- jadaRPGTimeline ".cred.toml"
  putStrLn $ displayTimelineColor timeline
  parseTest pTweet `mapM_` (timeline ^.. each . text) >>= print
