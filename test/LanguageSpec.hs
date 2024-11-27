module LanguageSpec where

import Language
import Test.Hspec

spec :: Spec
spec = do
  testSkip
  testInput
  testAssign
  testSequence
  testIf
  testWhile

-- Test for a simple skip command
testSkip :: Spec
testSkip = do
  let input = "skip"
      expected = Cskip
      result = reads input :: [(Command, String)]
  it "parses skip command" $ result `shouldBe` [(expected, "")]

-- Test for a simple input command
testInput :: Spec
testInput = do
  let input = "input x"
      expected = Cinput 'x'
      result = reads input :: [(Command, String)]
  it "parses input command" $ result `shouldBe` [(expected, "")]

-- Test for an assignment command
testAssign :: Spec
testAssign = do
  let input = "x := 5"
      expected = Cassign 'x' (Econst 5)
      result = reads input :: [(Command, String)]
  it "parses assignment command" $ result `shouldBe` [(expected, "")]

-- Test for a sequence of commands
testSequence :: Spec
testSequence = do
  let input = "skip; input x"
      expected = Cseq Cskip (Cinput 'x')
      result = reads input :: [(Command, String)]
  it "parses sequence of commands" $ result `shouldBe` [(expected, "")]

-- Test for an if command
testIf :: Spec
testIf = do
  let input = "if x <= 10 { skip; } else { input y;}input x"
      expected = Cseq (Cif (Rleq, 'x', 10) Cskip (Cinput 'y')) (Cinput 'x')
      result = reads input :: [(Command, String)]
  it "parses if command" $ result `shouldBe` [(expected, "")]
  let input = "if x <= 10 { skip; } else { input y; input x;}"
      expected = Cif (Rleq, 'x', 10) Cskip (Cseq (Cinput 'y') (Cinput 'x'))
      result = reads input :: [(Command, String)]
  it "parses if command" $ result `shouldBe` [(expected, "")]

-- Test for a while command
testWhile :: Spec
testWhile = do
  let input = "while x > 0{input y;} input x;"
      expected = Cseq (Cwhile (Rgt, 'x', 0) (Cinput 'y')) (Cinput 'x')
      result = reads input :: [(Command, String)]
  it "parses while command" $ result `shouldBe` [(expected, "")]
  let input = "while x > 0{input y; input x;}"
      expected = Cwhile (Rgt, 'x', 0) (Cseq (Cinput 'y') (Cinput 'x'))
      result = reads input :: [(Command, String)]
  it "parses while command" $ result `shouldBe` [(expected, "")]
