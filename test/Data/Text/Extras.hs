module Data.Text.Extras where

import Data.Text
import Test.QuickCheck

instance Arbitrary Text where
  arbitrary = pack <$> arbitrary
