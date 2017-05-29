module GlobSpec (main, spec) where

import Test.Hspec
import Glob

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parseGlob" $ do
    it "will parse an exact-match" $ do
      let matcher = compilePattern "directoryContents/"
      matcher `shouldBe` Matcher { isNegated = False, terms = [MatchLiteral "directoryContents/", MatchStarStar] }

      match' matcher "non-directoryContents/a" `shouldBe` False

    it "will match only the current directory" $ do
      compilePattern "foo" `shouldBe`
        Matcher { isNegated = False, terms = [MatchLiteral "foo"] }

    it "will match only the current directory" $ do
      compilePattern "/foo" `shouldBe`
        Matcher { isNegated = False, terms = [CurrentDirectory "foo"] }

    it "will match the contents of a directory" $ do
      compilePattern "foo/" `shouldBe`
        Matcher False [MatchLiteral "foo/", MatchStarStar]
      compilePattern "foo/bar/" `shouldBe`
        Matcher False [MatchLiteral "foo/bar/", MatchStarStar]
      compilePattern "foo/*/bar/" `shouldBe`
        Matcher False [MatchLiteral "foo/", MatchStar, MatchLiteral "/bar/", MatchStarStar]

    it "Will parse star patterns" $ do
      compilePattern "foo/*" `shouldBe`
        Matcher { isNegated = False, terms = [MatchLiteral "foo/", MatchStar] }
      compilePattern "foo/*/bar" `shouldBe`
        Matcher { isNegated = False, terms = [MatchLiteral "foo/", MatchStar, MatchLiteral "/bar"] }

    it "Will pick up negation" $ do
      compilePattern "foo" `shouldBe` Matcher { isNegated = False, terms = [MatchLiteral "foo"] }
      compilePattern "!foo" `shouldBe` Matcher { isNegated = True, terms = [MatchLiteral "foo"] }

  describe "match" $ do
    describe "exactChars" $ do
      let testPattern = "exactChars"

      it "exact match" $ do
        match testPattern "exactChars" `shouldBe` True
        match testPattern "./exactChars" `shouldBe` True

      it "as a file" $
        match testPattern "dir/exactChars" `shouldBe` True

      it "as a directory" $
        match testPattern "exactChars/someFile" `shouldBe` True

      it "as a nested directory" $
        match testPattern "foo/exactChars/asdas" `shouldBe` True

      it "filename contains pattern" $ do
        match testPattern "non-exactChars" `shouldBe` False
        match testPattern "./non-exactChars" `shouldBe` False

      it "nested dir - filename contains pattern" $ do
        match testPattern "dir/non-exactChars" `shouldBe` False
        match testPattern "./dir/non-exactChars" `shouldBe` False


    describe "/currentDirectory" $ do
      let testPattern = "/currentDirectory"
      it "Will match files only in the current directory" $
        match testPattern "currentDirectory" `shouldBe` True

      it "In current dir" $
        match testPattern "./currentDirectory" `shouldBe` True

      it "is directory with file" $
        match testPattern "currentDirectory/someFile" `shouldBe` False

      it "in nested directory" $
        match testPattern "foo/currentDirectory" `shouldBe` False

      it "is nested directory" $
        match testPattern "foo/currentDirectory/asdas" `shouldBe` False

      it "contains pattern" $ do
        match testPattern "non-currentDirectory" `shouldBe` False
        match testPattern "non-currentDirectory/a" `shouldBe` False

      it "contains pattern in current directory" $ do
        match testPattern "./non-currentDirectory" `shouldBe` False
        match testPattern "./non-currentDirectory/a" `shouldBe` False


    describe "directoryContents/" $ do
      let testPattern = "directoryContents/"

      it "file in directory" $ do
        match testPattern "directoryContents/a" `shouldBe` True
        match testPattern "./directoryContents/a" `shouldBe` True

      it "nested files" $ do
        match testPattern "directoryContents/a/b" `shouldBe` True
        match testPattern "./directoryContents/a/b" `shouldBe` True

      it "directory nested" $ do
        match testPattern "a/directoryContents/a" `shouldBe` True
        match testPattern "./a/directoryContents/a" `shouldBe` True

      it "directory contains name" $ do
        match testPattern "non-directoryContents/a" `shouldBe` False
        match testPattern "non-directoryContents/a/b" `shouldBe` False
        match testPattern "./non-directoryContents/a/b" `shouldBe` False


    describe "/currentDirectory/" $ do
      let testPattern = "/currentDirectory/"
      it "contains pattern" $ do
        match testPattern "currentDirectory/a" `shouldBe` True
        match testPattern "./currentDirectory/a" `shouldBe` True

      it "nested direcotry" $
        match testPattern "currentDirectory/a/b" `shouldBe` True

      it "contains pattern" $
        match testPattern "other-currentDirectory/a/b" `shouldBe` False

      it "not match files" $
        match testPattern "currentDirectory" `shouldBe` False

    describe "*" $ do
      let testPattern = "*"

      it "matches everything" $
        match testPattern "anything" `shouldBe` True

      it "in current directory with prefix" $
        match testPattern "./anything" `shouldBe` True

      it "not in current directory" $
        match testPattern "anything/foo" `shouldBe` True

    describe "' ' or '/'" $ do
      it "should match nothing" $ do
        match "" "anything" `shouldBe` False
        match "/" "anything" `shouldBe` False

    describe "*.extension" $ do
      let pattern = "*.a"

      it "in sub directory" $
        match pattern "foo.a" `shouldBe` True

      it "in sub directory" $
        match pattern "foo.b" `shouldBe` False


    describe "/*" $ do
      let testPattern = "/*"

      it "in current directory" $
        match testPattern "anything" `shouldBe` True

      it "in current directory with prefix" $
        match testPattern "./anything" `shouldBe` True

      it "not in current directory" $
        match testPattern "anything/foo" `shouldBe` False

    describe "/*.extension" $ do
      let testPattern = "/*.a"
      it "in current directory" $ do
        match testPattern "foo.a" `shouldBe` True
        match testPattern "./foo.a" `shouldBe` True
        match testPattern "./foo.b" `shouldBe` False

      it "in sub directory" $
        match testPattern "dir/foo.a" `shouldBe` False


    describe "**" $ do
      let testPattern = "**"
      it "matches everything" $ do
        match testPattern "a" `shouldBe` True
        match testPattern "./a" `shouldBe` True
        match testPattern "a/b" `shouldBe` True
        match testPattern "a/b/c.d" `shouldBe` True


    describe "**/*" $ do
      let testPattern = "**/*"
      it "matches everything" $ do
        match testPattern "a" `shouldBe` True
        match testPattern "./a" `shouldBe` True
        match testPattern "a/b" `shouldBe` True
        match testPattern "a/b/c.d" `shouldBe` True


    describe "**/*.ext" $ do
      let testPattern = "**/*.ext"
      it "matches" $
        match testPattern "foo.ext" `shouldBe` True

      it "matches in the current directory" $
        match testPattern "./foo.ext" `shouldBe` True

      it "matches at any depth" $
        match testPattern "a/b/c/d/e/foo.ext" `shouldBe` True

      it "will not match a directoy" $
        match testPattern "a/foo.ext/b" `shouldBe` False


    describe "negation" $ do
      it "!exactFileName" $ do
        let testPattern = "!exactFileName"
        match testPattern "exactFileName" `shouldBe` False
        match testPattern "foo/exactFileName" `shouldBe` False
        match testPattern "non-exactFileName" `shouldBe` True

      it "!*.a" $ do
        let testPattern = "!*.a"
        match testPattern "foo.a" `shouldBe` False
        match testPattern "foo.b" `shouldBe` True

    describe "matchesOneOf" $ do
      it "can handle exact matches" $
        matchesOneOf' ["foo"] "foo" `shouldBe` True

      it "can handle negations in exact matches" $ do
        matchesOneOf' ["!foo"] "foo" `shouldBe` False

      it "will use the last matching rule when multiple rules will match" $ do
        matchesOneOf' ["!foo", "foo"] "foo" `shouldBe` True
        matchesOneOf' ["foo", "!foo"] "foo" `shouldBe` False
