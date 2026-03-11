-- module ConfigSpec (spec) where

-- import Test.Tasty       (TestTree, testGroup)
-- import Test.Tasty.HUnit (testCase, (@?=))
-- import Dhall            (input, auto)
-- import Options.Generic  (unwrapRecord, unwrap)
-- import Config

-- spec :: TestTree
-- spec = testGroup "Config Tests"
  -- [ testCase "Dhall config matches CLI defaults" dhallMatchesDefaults
  -- ]

-- dhallMatchesDefaults :: IO ()
-- dhallMatchesDefaults = do
    -- cliWrapped <- unwrapRecord "<hattier3"     :: IO (Config Unwrapped)
    -- dhallConfig <- input auto "~/config.dhall" :: IO (Config Unwrapped)
    -- dhallConfig @?= cliDefaults
