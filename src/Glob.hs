module Glob (
  Matcher(..),
  MatchTerm(..),
  Pattern,
  match,
  match',
  matchTerms,
  matchesOneOf,
  matchesOneOf',
  compilePattern,
  ) where

import Data.List
import Data.Either (isRight)
import System.FilePath (pathSeparator)

type Pattern = String

data MatchTerm =
  CurrentDirectory String
  | MatchLiteral String
  | MatchStar
  | MatchStarStar
-- TODO handle the other globbing constructs
--   | OneOf [String]
--   | Chars [Char]
--   | SingleChar Char
  deriving (Eq, Show)

data Matcher =
  Matcher { isNegated :: Bool, terms :: [MatchTerm] }
  deriving (Eq, Show)

xor :: Bool -> Bool -> Bool
xor = (/=)


normalisePath :: FilePath -> FilePath
normalisePath ('.' : ('/' : rest)) = rest
normalisePath path = path


simplifyTerms :: [MatchTerm] -> [MatchTerm]
simplifyTerms [] = []
simplifyTerms (MatchLiteral [] : rest) = simplifyTerms rest
simplifyTerms (m@(CurrentDirectory a) : rest) =
  case simplifyTerms rest of
    (MatchLiteral b : bs) -> CurrentDirectory (a ++ b) : bs
    bs -> m : bs
simplifyTerms (m@(MatchLiteral a) : rest) =
  case simplifyTerms rest of
    (MatchLiteral b : bs) -> MatchLiteral (a ++ b) : bs
    bs -> m : bs
simplifyTerms (a:rest) = a:simplifyTerms rest


parseTerms :: Pattern -> [MatchTerm]
parseTerms [] = []
parseTerms ('*':'*':cs) = MatchStarStar : parseTerms cs
parseTerms ('*':cs) = MatchStar : parseTerms cs
parseTerms (c:[]) | c == pathSeparator = (MatchLiteral [pathSeparator]) : [MatchStarStar] -- TODO this doesn't work with the current strat for literal matching
parseTerms (c:cs) = MatchLiteral [c] : parseTerms cs


compilePattern :: Pattern -> Matcher
compilePattern [] = Matcher False []
compilePattern ['*'] = Matcher False [MatchStarStar]  -- A pattern of '*' at the top level should match everything
compilePattern (pattern@(controlChar:rest))
  | controlChar == '!' = Matcher True (simplifyTerms restTerms)
  | controlChar == pathSeparator = Matcher False (simplifyTerms $ (CurrentDirectory []):restTerms)
  | otherwise = Matcher False (simplifyTerms (parseTerms pattern))
  where restTerms = parseTerms rest


data MatchFailure =
  ResidualCharacters |
  NonMatching String |
  NoMatch |
  NotAPrefix |
  PathSeparator |
  BadParse String
  deriving (Show)

matchTerms :: [MatchTerm] -> FilePath -> Either MatchFailure ()

matchTerms [] [] = return ()
matchTerms [] _ = Left ResidualCharacters


-- abc
-- abc, abc/foo, foo/abc, foo/abc/bar
-- abc/def
-- abc/def, abc/def/foo, foo/abc/def, foo/abc/def/bar
matchTerms [MatchLiteral _] [] = Left NotAPrefix
matchTerms [MatchLiteral lit] path
  | lit == path = return ()
  | isPrefixOf (lit ++ [pathSeparator]) path = return ()
  | isSuffixOf (pathSeparator:lit) path = return ()
  | isInfixOf ([pathSeparator] ++ lit ++ [pathSeparator]) path = return ()
  | otherwise = Left NoMatch

matchTerms (allTerms@((MatchLiteral literalString) : remainingTerms)) path =
  matchLiteral literalString path
    where
      matchLiteral [] unMatchedPath = matchTerms remainingTerms unMatchedPath
      matchLiteral (x : xs) (y : ys)
        | x == y = matchLiteral xs ys
        | x /= y = matchTerms allTerms ys -- TODO this is wrong it allows "abc/" to match "foo-abc/bar" as ending in a / appends ** to the pattern
      matchLiteral _ _ = Left NotAPrefix

-- /abc
-- abc, abc/foo
-- /abc/def
-- abc/def, abc/def/foo
matchTerms [CurrentDirectory _] [] = Left NotAPrefix
matchTerms ((CurrentDirectory pattern): restTerms) filePath
  | pattern == filePath = return ()
  | isPrefixOf pattern filePath = matchTerms restTerms (drop (length pattern) filePath)
  | otherwise = Left NotAPrefix


matchTerms [MatchStarStar] _ = return ()
matchTerms (MatchStarStar : _) [] = Left NoMatch
matchTerms (MatchStarStar : remainingTerms) filePath =
  matchAny filePath
    where
      matchAny [] = return ()
      matchAny path = case matchTerms remainingTerms path of
        Left _ -> matchAny (drop 1 path)
        _ -> matchTerms (drop 1 remainingTerms) (drop 1 path)

matchTerms [MatchStar] path
  | pathSeparator `elem` path = Left PathSeparator
  | otherwise = return ()

matchTerms (MatchStar:ts) filePath =
  matchDir filePath
    where
      matchDir [] = Left NoMatch
      matchDir (c:_) | c == pathSeparator = Left PathSeparator
      matchDir path =
        case matchTerms ts path of
          Left _ -> matchDir $ tail path
          _ -> matchTerms ts path


match :: Pattern -> FilePath -> Bool
match rawPattern path = match' (compilePattern rawPattern) path

matchesTerms :: Matcher -> FilePath -> Bool
matchesTerms matcher path =
  (isRight . matchTerms (terms matcher) . normalisePath) path

match' :: Matcher -> FilePath -> Bool
match' matcher path =
  (isNegated matcher) `xor` (matchesTerms matcher (normalisePath path))

matchesOneOf :: [Matcher] -> FilePath -> Bool
matchesOneOf matchers path = case find (\matcher -> matchesTerms matcher path) (reverse matchers) of
  Just matcher -> not (isNegated matcher)
  Nothing -> False


matchesOneOf' :: [Pattern] -> FilePath -> Bool
matchesOneOf' patterns = matchesOneOf $ compilePattern <$> patterns
