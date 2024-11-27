module IntervalAbstractionSpec where

import Abstraction
import IntervalAbstraction
import Language
import Memory
import Test.Hspec

spec :: Spec
spec = do
  describe "analyzeExpr" $ do
    it "analyzes constant expressions" $ do
      analyzeExpr (Econst 5) initMemory `shouldBe` Interval (Int 5) (Int 5)

    it "analyzes variable expressions" $ do
      let mem = setVariable 'x' (Interval (Int 1) (Int 10)) initMemory
      analyzeExpr (Evar 'x') mem `shouldBe` Interval (Int 1) (Int 10)

    it "analyzes addition of intervals" $ do
      let mem = setVariable 'x' (Interval (Int 1) (Int 2)) initMemory
      let mem' = setVariable 'y' (Interval (Int 3) (Int 4)) mem
      analyzeExpr (Ebop Badd (Evar 'x') (Evar 'y')) mem' `shouldBe` createInterval (Int 4) (Int 6)

  describe "analyzeCommand" $ do
    it "analyzes assignment commands" $ do
      let mem = analyzeCommand (Cassign 'x' (Econst 5)) initMemory
      getVariable 'x' mem `shouldBe` Interval (Int 5) (Int 5)

    it "analyzes skip commands" $ do
      let x = (Interval (Int 1) (Int 10))
      let mem = analyzeCommand Cskip (setVariable 'x' x initMemory)
      getVariable 'x' mem `shouldBe` x

    it "analyzes sequential commands" $ do
      let mem = analyzeCommand (Cseq (Cassign 'x' (Econst 5)) (Cassign 'y' (Econst 10))) initMemory
      getVariable 'x' mem `shouldBe` Interval (Int 5) (Int 5)
      getVariable 'y' mem `shouldBe` Interval (Int 10) (Int 10)

    it "analyzes if commands" $ do
      let x = (Interval (Int 6) (Int 10))
      let mem = analyzeCommand (Cif (Rgt, 'x', 5) (Cassign 'y' (Econst 10)) (Cassign 'y' (Econst 20))) (setVariable 'x' x initMemory)
      getVariable 'y' mem `shouldBe` Interval (Int 10) (Int 10)
      let mem = analyzeCommand (Cif (Rleq, 'x', 5) (Cassign 'y' (Econst 10)) (Cassign 'y' (Econst 20))) (setVariable 'x' x initMemory)
      getVariable 'y' mem `shouldBe` Interval (Int 20) (Int 20)

    it "analyzes if commands" $ do
      let x = (Interval (Int 0) (Int 5))
      let mem = analyzeCommand (Cif (Rgt, 'x', 5) (Cassign 'y' (Econst 10)) (Cassign 'y' (Econst 20))) (setVariable 'x' x initMemory)
      getVariable 'y' mem `shouldBe` Interval (Int 20) (Int 20)
      let mem = analyzeCommand (Cif (Rleq, 'x', 5) (Cassign 'y' (Econst 10)) (Cassign 'y' (Econst 20))) (setVariable 'x' x initMemory)
      getVariable 'y' mem `shouldBe` Interval (Int 10) (Int 10)

    it "analyzes if commands" $ do
      let x = (Interval (Int 0) (Int 10))
      let mem = analyzeCommand (Cif (Rgt, 'x', 5) (Cassign 'y' (Econst 10)) (Cassign 'y' (Econst 20))) (setVariable 'x' x initMemory)
      getVariable 'y' mem `shouldBe` Interval (Int 10) (Int 20)
      let mem = analyzeCommand (Cif (Rleq, 'x', 5) (Cassign 'y' (Econst 10)) (Cassign 'y' (Econst 20))) (setVariable 'x' x initMemory)
      getVariable 'y' mem `shouldBe` Interval (Int 10) (Int 20)

    it "analyzes while commands" $ do
      let mem = analyzeCommand (Cseq (Cassign 'x' (Econst 5)) (Cwhile (Rgt, 'x', 0) (Cassign 'x' (Ebop Bsub (Evar 'x') (Econst 1))))) initMemory
      getVariable 'x' mem `shouldBe` Interval NegInf (Int 0)

    it "analyzes while commands" $ do
      let mem = analyzeCommand (Cseq (Cassign 'x' (Econst 5)) (Cwhile (Rleq, 'x', 0) (Cassign 'x' (Ebop Bsub (Evar 'x') (Econst 1))))) initMemory
      getVariable 'x' mem `shouldBe` Interval (Int 5) (Int 5)
