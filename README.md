# jada's journey
following the journey of [JadaRpg](https://twitter.com/JadaRpg)

# motivation
to appreciate a whimsical bot, and to parse something fun

# implementation
the tweets behind the [JadaRpg](https://twitter.com/JadaRpg) account are very structured:
> Jada *hastily* killed a *Gonzoulon* and reached *level 3*!

by defining types for the tweets,
``` haskell
data JadaTweet
  = Discovery String
  | Train String
  | Enemy Kill
  | Flavor String
  deriving (Show)
  
data Kill = Kill
  { killStyle :: String,
    killTarget :: String,
    killReward :: Reward
  }
  deriving (Show)
```

and parsers for each type:
``` haskell
pKill :: Parser JadaTweet
pKill = dbg "kill" $ do
  string "Jada "
  killStyle <- lexeme $ some letterChar
  string "killed a "
  killTarget <- lexeme $ some alphaNumChar
  killReward <- try pLevel <|> try pItem
  return $ Enemy $ Kill {killStyle, killTarget, killReward}
```

we can get some values representing each tweet:
```haskell 
kill> IN: "Jada hastily killed a Gonzoulon and rea <…>
kill> MATCH (COK): "Jada hastily killed a Gonzoulon and rea <…>
kill> VALUE: Enemy (Kill {killStyle = "hastily", killTarget = "Gonzoulon", killReward = Level "3"})
```

once we have a way to process tweets into structured information, we can ask questions about the entire journey!

> what item rewards has Jada collected so far?

``` haskell
do
  timeline <- jadaRPGTimeline ".cred.toml";
  let 
    rewards :: String -> Maybe Reward
    rewards = \case parseMaybe pTweet of
        Just (Enemy e)) -> Just (killReward e)
        otherwise -> Nothing
  in pure . mapMaybe rewards $ timeline ^.. each . text
```

``` text
Item "Garbage Outer Sock"
Item "Poor Blunt Hatchet"
Item "Fine Tiered Tiara"
Item "Terrible Wear Earmuffs"
Item "Good Grotesque Tiara"
Item "Poor Giant Frisbee"
Item "Fine Antitrust Suit"
Item "Garbage Blossom Necklace "
Item "Terrible Seventh Greaves"
Item "Legendary Large Hatchet"
Item "Terrible Wintry Flail"
Item "Fine Ancient Guisarme"
```

# future work
* export parsed data (JSON)
* simple visualization with [diagrams](https://diagrams.github.io/)
* deep generative model for art?
* generate item descriptions with GPT-3?
