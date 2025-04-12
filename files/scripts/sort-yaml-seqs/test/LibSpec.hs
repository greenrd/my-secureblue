module LibSpec (spec) where

import Hedgehog.Classes
import Skeletest
import qualified Skeletest.Prop.Gen as Gen
import qualified Skeletest.Prop.Range as Range
import Text.Libyaml (Tag(..))
import Lib ()

genTag :: Gen Tag
genTag = Gen.choice [pure StrTag, pure FloatTag, pure NullTag, pure BoolTag, pure SetTag, pure IntTag, pure SeqTag, pure MapTag,
         UriTag <$> Gen.list (Range.linear 0 50) Gen.ascii]

spec :: Spec
spec = do
  describe "Ord Tag instance" $ do
    it "should follow the Ord laws" $ do
      result <- lawsCheck $ ordLaws genTag
      result `shouldBe` True