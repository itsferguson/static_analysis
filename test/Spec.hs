import IntervalAbstractionSpec
import LanguageSpec
import SignAbstractionSpec
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Parser" LanguageSpec.spec -- Run tests from MyModuleSpec
  describe "SignAbstraction" SignAbstractionSpec.spec -- Run tests from MyModuleSpec
  describe "IntervalAbstraction" IntervalAbstractionSpec.spec -- Run tests from MyModuleSpec
