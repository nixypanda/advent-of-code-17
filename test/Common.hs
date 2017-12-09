{-# LANGUAGE RecordWildCards #-}

module Common
  ( TestDefinition(..)
  , apply
  ) where
import Test.HUnit


type TestDescription = String
type FailureMessage  = String
data TestDefinition i o = TD
  { desc :: TestDescription
  , msg :: FailureMessage
  , input :: i
  , output :: o
  }


apply :: (Eq o, Show o) => (i -> o) -> TestDefinition i o -> Test
apply f TD{..} =
  TestLabel desc $ TestCase $ assertEqual msg output (f input)

