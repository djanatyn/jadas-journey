{-# LANGUAGE NamedFieldPuns #-}

module Jada.Journey
  ( main,
  )
where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L (lexeme)
import Text.Megaparsec.Debug (dbg)
import Web.Tweet
import Web.Tweet.API
import Web.Tweet.Utils

type Parser = Parsec Void String

data Kill = Kill
  { killStyle :: String,
    killTarget :: String,
    killReward :: Reward
  }
  deriving (Show)

data Reward = Level String | Item String deriving (Show)

newtype Discovery = Discovery String deriving (Show)

newtype Train = Train String deriving (Show)

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

pKill :: Parser Kill
pKill = dbg "kill" $ do
  string "Jada "
  killStyle <- lexeme $ some letterChar >> string "killed a "
  killTarget <- lexeme $ some alphaNumChar
  killReward <- pLevel <|> pItem

  return Kill {killStyle, killTarget, killReward}

pDiscovery :: Parser Discovery
pDiscovery = do
  string "Jada has discovered the "
  discovery <- someTill asciiChar (char '.')
  return $ Discovery discovery

pTrain :: Parser Train
pTrain = do
  string "Jada notices a train headed for the "
  destination <- someTill asciiChar (char '.')
  return $ Train destination

pFlavor :: Parser String
pFlavor =
  choice $
    string
      <$> [ "Entering the world of... Jada MMORPG",
            "Jada has entered the world at level 1!",
            "The finest finery for supporters of the Beetle King and his adventures. https://t.co/xJuUWWmnpY",
            "Jada's bard finally learned how to sing."
          ]

jadaRPGTimeline :: FilePath -> IO Timeline
jadaRPGTimeline = getAll "jadaRPG" Nothing

main :: IO ()
main = putStrLn "success"
