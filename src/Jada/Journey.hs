{-# LANGUAGE NamedFieldPuns #-}

module Jada.Journey
  ( main,
  )
where

import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L (lexeme)
import Text.Megaparsec.Debug (dbg)
import Web.Tweet
import Web.Tweet.API
import Web.Tweet.Utils

type Parser = Parsec Void String

data Kill = Kill
  { killStyle :: [String],
    killTarget :: [String]
  }
  deriving (Show)

lexeme = L.lexeme space

pStatement :: Parser Kill
pStatement = dbg "statement" $ do
  _ <- string "Jada "
  killStyle <- someTill (lexeme $ some letterChar) (lexeme $ string "killed")
  _ <- lexeme $ string "a"
  killTarget <- someTill (lexeme $ some letterChar) (lexeme $ string "and")
  return Kill {killStyle, killTarget}

jadaRPGTimeline :: FilePath -> IO Timeline
jadaRPGTimeline = getAll "jadaRPG" Nothing

main :: IO ()
main = do
  putStrLn "success"

--- jadaRPGTimeline ".cred.toml" >>= putStrLn . displayTimeline
