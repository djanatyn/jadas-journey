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

jadaRPGTimeline :: FilePath -> IO Timeline
jadaRPGTimeline = getAll "jadaRPG" Nothing

pFlavor :: Parser String
pFlavor =
  choice $
    string
      <$> [ "Entering the world of... Jada MMORPG",
            "Jada has entered the world at level 1!"
          ]

main :: IO ()
main = do
  putStrLn "success"

--- jadaRPGTimeline ".cred.toml" >>= putStrLn . displayTimeline
