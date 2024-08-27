module System.Console.Haskeline.Style
  ( Color(..)
  , StyleAttr(..)
  , StyleAttrs(..)
  , StyledText(..)
  , decay
  ) where

data Color
  = Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White
  | ColorNumber Int
  deriving (Show, Eq, Ord)

data StyleAttr
  = WithFgColor Color
  | WithBgColor Color
  deriving (Show, Eq, Ord)

newtype StyleAttrs
  = StyleAttrs { unStyleAttrs :: [StyleAttr] }
  deriving (Show, Eq)

data (Show s, Eq s) => StyledText s
  = PlainText s
  | FancyText StyleAttrs s
  | FancyTextGroup [StyledText s]
  deriving (Show, Eq)

instance (Show s, Eq s, Semigroup s) => Semigroup (StyledText s) where
  PlainText s1 <> PlainText s2 = PlainText $ s1 <> s2
  t1@(PlainText _) <> t2@(FancyText _ _) = FancyTextGroup [t1, t2]
  t1@(PlainText _) <> FancyTextGroup [] = FancyTextGroup [t1]
  t1@(PlainText _) <> FancyTextGroup ss = FancyTextGroup $ concatWithGroup t1 ss
  t1@(FancyText _ _) <> t2@(PlainText _) = FancyTextGroup [t1, t2]
  t1@(FancyText a1 s1) <> t2@(FancyText a2 s2)
    | a1 == a2 = FancyText a1 $ s1 <> s2
    | otherwise = FancyTextGroup [t1, t2]
  t1@(FancyText _ _) <> FancyTextGroup ss = FancyTextGroup $ concatWithGroup t1 ss
  FancyTextGroup ss <> t2@(PlainText _) = FancyTextGroup $ groupConcatWith ss t2
  FancyTextGroup ss <> t2@(FancyText _ _) = FancyTextGroup $ groupConcatWith ss t2
  FancyTextGroup [] <> FancyTextGroup ss2 = FancyTextGroup ss2
  FancyTextGroup ss1 <> FancyTextGroup [] = FancyTextGroup ss1
  FancyTextGroup ss1 <> FancyTextGroup (s2:ss2) =
    FancyTextGroup $ groupConcatWith ss1 s2 <> ss2

-- | Decay fancy text back to string
decay :: (Show s, Eq s, Semigroup s, Monoid s)
  => StyledText s
  -> s
decay (PlainText s) = s
decay (FancyText _ s) = s
decay (FancyTextGroup ss) = foldl (<>) mempty . map decay $ ss

concatWithGroup
  :: (Show s, Eq s, Semigroup s)
  => StyledText s
  -> [StyledText s]
  -> [StyledText s]
concatWithGroup t [] = [t]
concatWithGroup t (s:ss) = case t <> s of
  FancyTextGroup p -> p <> ss
  t' -> t':ss

groupConcatWith
  :: (Show s, Eq s, Semigroup s)
  => [StyledText s]
  -> StyledText s
  -> [StyledText s]
groupConcatWith [] t = [t]
groupConcatWith ss t = case last ss <> t of
  FancyTextGroup p -> init ss <> p
  t' -> init ss <> [t']
