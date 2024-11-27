module SignAbstractionSpec where

import Abstraction
import Language
import Memory
import SignAbstraction
import Test.Hspec

spec :: Spec
spec = do
  describe "Analyze Expressions" testAnalyzeExpr
  describe "Analyze Commands" testAnalyzeCommand

testAnalyzeExpr :: Spec
testAnalyzeExpr = do
  it "analyzes constant positive values" $ do
    analyzeExpr (Econst 5) initMemory `shouldBe` Apos

  it "analyzes constant negative values" $ do
    analyzeExpr (Econst (-3)) initMemory `shouldBe` Aneg

  it "analyzes variable values" $ do
    let mem = setVariable 'x' Apos initMemory
    analyzeExpr (Evar 'x') mem `shouldBe` Apos

  it "analyzes addition of two positive values" $ do
    let mem = setVariable 'x' Apos . setVariable 'y' Apos $ initMemory
    analyzeExpr (Ebop Badd (Evar 'x') (Evar 'y')) mem `shouldBe` Apos

  it "analyzes addition of two negative values" $ do
    let mem = setVariable 'x' Aneg . setVariable 'y' Aneg $ initMemory
    analyzeExpr (Ebop Badd (Evar 'x') (Evar 'y')) mem `shouldBe` Aneg

  it "analyzes addition of positive and negative values" $ do
    let mem = setVariable 'x' Aneg . setVariable 'y' Apos $ initMemory
    analyzeExpr (Ebop Badd (Evar 'x') (Evar 'y')) mem `shouldBe` Atop
    analyzeExpr (Ebop Badd (Evar 'y') (Evar 'x')) mem `shouldBe` Atop

  it "analyzes addition with Abot" $ do
    let mem = setVariable 'x' Abot initMemory
    analyzeExpr (Ebop Badd (Evar 'x') (Econst 5)) mem `shouldBe` Abot
    analyzeExpr (Ebop Badd (Econst 5) (Evar 'x')) mem `shouldBe` Abot

  it "analyzes addition with Atop" $ do
    let mem = setVariable 'x' Atop initMemory
    analyzeExpr (Ebop Badd (Evar 'x') (Econst 5)) mem `shouldBe` Atop
    analyzeExpr (Ebop Badd (Econst 5) (Evar 'x')) mem `shouldBe` Atop

  it "analyzes subtraction of two positive values" $ do
    let mem = setVariable 'x' Apos . setVariable 'y' Apos $ initMemory
    analyzeExpr (Ebop Bsub (Evar 'x') (Evar 'y')) mem `shouldBe` Atop

  it "analyzes subtraction of two negative values" $ do
    let mem = setVariable 'x' Aneg . setVariable 'y' Aneg $ initMemory
    analyzeExpr (Ebop Bsub (Evar 'x') (Evar 'y')) mem `shouldBe` Atop

  it "analyzes subtraction of positive and negative values" $ do
    let mem = setVariable 'x' Aneg . setVariable 'y' Apos $ initMemory
    analyzeExpr (Ebop Bsub (Evar 'x') (Evar 'y')) mem `shouldBe` Aneg
    analyzeExpr (Ebop Bsub (Evar 'y') (Evar 'x')) mem `shouldBe` Apos

  it "analyzes subtraction with Abot" $ do
    let mem = setVariable 'x' Abot initMemory
    analyzeExpr (Ebop Bsub (Evar 'x') (Econst 5)) mem `shouldBe` Abot
    analyzeExpr (Ebop Bsub (Econst 5) (Evar 'x')) mem `shouldBe` Abot

  it "analyzes subtraction with Atop" $ do
    let mem = setVariable 'x' Atop initMemory
    analyzeExpr (Ebop Bsub (Evar 'x') (Econst 5)) mem `shouldBe` Atop
    analyzeExpr (Ebop Bsub (Econst 5) (Evar 'x')) mem `shouldBe` Atop

testAnalyzeCommand :: Spec
testAnalyzeCommand = do
  it "analyzes assignment of a positive value" $ do
    let mem = analyzeCommand (Cassign 'x' (Econst 5)) initMemory
    getVariable 'x' mem `shouldBe` Apos

  it "analyzes assignment of a negative value" $ do
    let mem = analyzeCommand (Cassign 'y' (Econst (-3))) initMemory
    getVariable 'y' mem `shouldBe` Aneg

  it "analyzes skip command" $ do
    let mem = analyzeCommand Cskip initMemory :: Memory SignAbstraction
    mem `shouldBe` initMemory

  it "analyzes input command" $ do
    let mem = analyzeCommand (Cinput 'x') initMemory
    getVariable 'x' mem `shouldBe` Atop

  it "analyzes sequential commands" $ do
    let mem = analyzeCommand (Cseq (Cassign 'x' (Econst 5)) (Cassign 'y' (Econst (-3)))) initMemory
    getVariable 'x' mem `shouldBe` Apos
    getVariable 'y' mem `shouldBe` Aneg

  it "analyzes if command with true condition" $ do
    let cmd = Cseq (Cassign 'x' (Econst 5)) (Cif (Rgt, 'x', -5) (Cassign 'y' (Econst 5)) (Cassign 'y' (Econst (-3))))
    let mem = analyzeCommand cmd initMemory
    getVariable 'y' mem `shouldBe` Apos

  it "analyzes if command with false condition" $ do
    let cmd = Cseq (Cassign 'x' (Econst 5)) (Cif (Rleq, 'x', -5) (Cassign 'y' (Econst 5)) (Cassign 'y' (Econst (-3))))
    let mem = analyzeCommand cmd initMemory
    getVariable 'y' mem `shouldBe` Aneg

  it "analyzes while command" $ do
    let cmd = Cseq (Cassign 'x' (Econst 5)) (Cwhile (Rgt, 'x', -5) (Cassign 'x' (Ebop Bsub (Evar 'x') (Econst 1))))
    let mem = analyzeCommand cmd initMemory
    getVariable 'x' mem `shouldBe` Aneg

  it "analyzes while with false condition" $ do
    let cmd = Cseq (Cassign 'x' (Econst 5)) (Cwhile (Rleq, 'x', -5) (Cassign 'x' (Ebop Bsub (Evar 'x') (Econst 1))))
    let mem = analyzeCommand cmd initMemory
    getVariable 'x' mem `shouldBe` Apos
