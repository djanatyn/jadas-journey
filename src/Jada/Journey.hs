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
  killReward <- pLevel <|> pItem

  return $ Enemy $ Kill {killStyle, killTarget, killReward}

pDiscovery :: Parser JadaTweet
pDiscovery = dbg "discovery" $ do
  string "Jada has discovered the "
  discovery <- lexeme $ someTill printChar (char '.')
  lexeme . string $ "The monsters here are unforgiving but the Beetle King will persevere."
  lexeme . string $ "He makes camp for the night as he prepares to leave the"
  oldPlace <- lexeme $ someTill printChar (char '.')
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
      string
        <$> [ "Entering the world of... Jada MMORPG",
              "Jada has entered the world at level 1!",
              "The finest finery for supporters of the Beetle King and his adventures. https://t.co/xJuUWWmnpY",
              "Jada's bard finally learned how to sing."
            ]
  return $ Flavor text

pTweet :: Parser JadaTweet
pTweet =
  try pDiscovery
    <|> try pFlavor
    <|> try pKill
    <|> try pDiscovery
    <|> pTrain

jadaRPGTimeline :: FilePath -> IO Timeline
jadaRPGTimeline = getAll "jadaRPG" Nothing

main :: IO ()
main = do
  timeline <- jadaRPGTimeline ".cred.toml"
  putStrLn $ displayTimelineColor timeline
  parseTest pTweet `mapM_` (timeline ^.. each . text) >>= print
