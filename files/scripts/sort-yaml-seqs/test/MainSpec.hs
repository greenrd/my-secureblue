module MainSpec (spec) where

import Skeletest
import System.Process (callProcess)

spec :: Spec
spec = do
  describe "sort-yaml-seqs" $ do
    it "sorts YAML sequences in the input" $
      let
        inputF = "test/recipe.yml"
        outputF = "/tmp/recipe2.yml"
        expectedF = "test/recipe-sorted.yml"
      in do
        callProcess "sort-yaml-seqs-exe" [inputF, outputF]
        output <- readFile outputF
        expected <- readFile expectedF
        output `shouldBe` expected