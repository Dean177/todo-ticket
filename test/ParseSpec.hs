module ParseSpec where

import Test.Hspec

import Parse
import Types

testFile :: String
testFile =
  "\n" ++
  "some content" ++ "\n" ++
  "\n" ++
  "// todo test1 2" ++ "\n" ++
  "abc" ++ "\n" ++
  "some content // TODO" ++ "\n" ++
 "\n" ++
  "efg" ++ "\n" ++
 "\n" ++
  "/* TODO ticket-123 asdkjasd oiansdoaisnd" ++ "\n" ++
 "\n" ++
 "\n" ++
  "asd" ++ "\n" ++
 "\n" ++
 "\n" ++
  "asd" ++ "\n" ++
 "\n" ++
  "// Todo ticket-456" ++ "\n" ++
  "asd" ++ "\n" ++
   "\n"

testFileName :: String
testFileName = "./test-file"

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Parse" $
  it "Can extract TODOS from a string" $
    parseTodos testFileName testFile `shouldBe` Right [
      Todo {file = testFileName, line = 5, column = 1, comment = "test1 2"},
      Todo {file = testFileName, line = 7, column = 1, comment = ""},
      Todo {file = testFileName, line = 19, column = 1, comment = "ticket-456"}
    ]