# jada's journey
following the journey of [JadaRpg](https://twitter.com/JadaRpg)

# motivation
to appreciate a whimsical bot, and to parse something fun

# implementation
the tweets behind the [JadaRpg](https://twitter.com/JadaRpg) account are very structured:
> Jada *hastily* killed a *Gonzoulon* and reached *level 3*!

by defining types for the tweets,
``` haskell
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
```

and parsers for each type:
``` haskell
pKill :: Parser JadaTweet
pKill = dbg "kill" $ do
  string "Jada "
  style <- lexeme $ some letterChar
  string "killed a "
  target <- lexeme $ some alphaNumChar
  reward <- try pLevel <|> try pItem
  return $ Enemy $ Kill {style, target, reward}
```

we can get some values representing each tweet:
```haskell 
kill> IN: "Jada kiddingly killed a Attendedrius an <…>
kill> MATCH (COK): "Jada kiddingly killed a Attendedrius an <…>
kill> VALUE: Enemy (Kill {style = "kiddingly", target = "Attendedrius", reward = Level "2"})
```

once we have a way to process tweets into structured information, we can ask questions about the entire journey!

> what item rewards has Jada collected so far?

``` haskell
do
  timeline <- jadaRPGTimeline ".cred.toml";
  let 
    rewards :: String -> Maybe Reward
    rewards = \case parseMaybe pTweet of
        Just (Enemy e) -> Just (killReward e)
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
