module Types(Todo(..)) where

data Todo = Todo {
  file :: FilePath,
  line :: Word,
  column :: Word,
  comment :: String
} deriving (Eq, Show)
