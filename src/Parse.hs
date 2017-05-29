module Parse where

import Data.Maybe (catMaybes)
import Text.Megaparsec (
  Dec,
  ParseError,
  SourcePos(..),
  (<|>),
  anyChar,
  eol,
  getPosition,
  many,
  manyTill,
  parse,
  separatorChar,
  skipMany,
  string,
  string',
  try,
  unPos
  )
import Text.Megaparsec.String (Parser)
import Types

todoStartToken :: Parser ()
todoStartToken =
  string "//" >>
  skipMany separatorChar >>
  string' "TODO" >>
  skipMany separatorChar

todo :: Parser Todo
todo = do
  commentContent <- anyChar `manyTill` eol
  (SourcePos name line' column') <- getPosition
  return Todo {
    file = name,
    line = unPos line',
    column = unPos column',
    comment = commentContent
  }

todoParser :: Parser (Maybe Todo)
todoParser =
  try (anyChar `manyTill` todoStartToken *> (Just <$> todo))
  <|> (anyChar `manyTill` eol *> return Nothing)


parseTodos :: FilePath -> String -> Either (ParseError Char Dec) [Todo]
parseTodos filePath input =
  catMaybes <$> parse (many todoParser) filePath input

todosFromFile :: FilePath -> IO (Either (ParseError Char Dec) [Todo])
todosFromFile path = do
  fileContent <- readFile path
  return $ catMaybes <$> parse (many todoParser) path fileContent
