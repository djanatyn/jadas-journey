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

data Kill = Kill {killStyle :: [String]} deriving (Show)

pStatement :: Parser Kill
pStatement = dbg "statement" $ do
  _ <- string "Jada "
  killStyle <- someTill (L.lexeme space (some letterChar)) (string "killed")
  return Kill {killStyle}

jadaRPGTimeline :: FilePath -> IO Timeline
jadaRPGTimeline = getAll "jadaRPG" Nothing

main :: IO ()
main = do
  putStrLn "success"

--- jadaRPGTimeline ".cred.toml" >>= putStrLn . displayTimeline
