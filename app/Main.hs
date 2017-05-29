{-#  OPTIONS_GHC -fno-warn-type-defaults #-}
{-#  LANGUAGE OverloadedStrings #-}
module Main where

import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import System.FilePath ((</>), isDrive, takeDirectory)
import System.Directory (doesDirectoryExist, doesFileExist, getCurrentDirectory)

import qualified Conduit

import Parse
import qualified Glob

type IgnoreRules = [Glob.Matcher]

recursiveUpSearch :: String -> FilePath -> IO (Maybe FilePath)
recursiveUpSearch fileName directoryPath = do
  containsConfigFile <- doesFileExist filePath
  if containsConfigFile
  then return (Just filePath)
  else if isDrive parentDirectory
  then return Nothing
  else recursiveUpSearch fileName parentDirectory
  where
    filePath = directoryPath </> fileName
    parentDirectory = takeDirectory directoryPath

parseConfigFile :: FilePath -> IO IgnoreRules
parseConfigFile filePath = foldr ((:) . Glob.compilePattern ) ([] :: IgnoreRules) <$> (lines <$> readFile filePath)

defaultRules :: IgnoreRules
defaultRules = Glob.compilePattern <$> [".git"]

readConfig :: IO IgnoreRules
readConfig = do
  currentDir <- getCurrentDirectory
  gitRules <- fmap parseConfigFile <$> recursiveUpSearch ".gitignore" currentDir
  todoRules <- fmap parseConfigFile <$> recursiveUpSearch ".todoignore" currentDir

  fromMaybe (pure defaultRules) (Just (pure defaultRules) <> gitRules <> todoRules)



isTextFile :: FilePath -> IO Bool
isTextFile filePath = not <$> doesDirectoryExist filePath

processFile :: FilePath -> IO ()
processFile fp = do
  print ""
  print fp
  todosFromFile fp >>= print
  print ""
  return ()
--  do
--    res <- todosFromFile fp
--    print res


main :: IO ()
main = do
  rules <- readConfig
  let path = ".stack-work/dist"
  print $ filter (`Glob.match'` path) rules
  let filterIncludedFiles = Conduit.filterC (not . Glob.matchesOneOf rules)
  let source = Conduit.sourceDirectoryDeep False "."
  Conduit.runResourceT (source Conduit..|
    filterIncludedFiles Conduit.$$
    Conduit.mapM_C (Conduit.liftIO . processFile))
  return ()

