-- Some disabled warnings to delete after recursive printing with indentation is implemented
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Integration.Module
  ( tests,
  )
where

import Data.Text
import Data.Text qualified as T
import Integration.Helpers (runFullFormatter)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))

tests :: TestTree
tests =
  testGroup
    "Module integration tests"
    -- TODO: enable this test after recursive printing with indentation is implemented:
    -- [testCase "PrimaryAlignment: correctly align a module with deep nesting" largeModuleWithNesting]
    []

largeModuleWithNesting :: IO ()
largeModuleWithNesting = expected @=? runFullFormatter input

input :: Text
input =
  T.unlines
    [ "module IntegrationTest where",
      "",
      "f :: Int -> Int -> Int",
      "f x y =",
      "  let a = case x of",
      "        0 -> 1",
      "        n -> let b = n + y",
      "             in case b of",
      "                  0 -> 0",
      "                  k -> k + a",
      "  in a + y",
      "",
      "g :: [Int] -> Int",
      "g [] = 0",
      "g (x:xs) =",
      "  let val = case x of",
      "        0 -> 10",
      "        n -> n * 2",
      "      rest = case xs of",
      "        [] -> 0",
      "        ys -> let inner = case y of",
      "                          _ -> length ys",
      "              in inner",
      "  in val + rest",
      "",
      "h :: Int -> Int",
      "h x",
      "  | x == 0 = 1",
      "  | x > 0 =",
      "      let pos = case x of",
      "            1 -> 1",
      "            n -> n * 2",
      "      in pos",
      "  | otherwise =",
      "      let neg = case x of",
      "            _ -> -x",
      "      in neg",
      "",
      "complex :: Int -> Int -> Int -> Int",
      "complex a b c =",
      "  let first = case a of",
      "        0 -> let innerA = case b of",
      "                          0 -> 0",
      "                          n -> n + 1",
      "             in innerA",
      "        x -> x",
      "      second = case (b, c) of",
      "        (0, _) -> 0",
      "        (_, 0) -> let innerB = case a of",
      "                                  0 -> 1",
      "                                  n -> n",
      "                  in innerB",
      "        (m, n) -> m + n",
      "      third = let t = case first of",
      "                    0 -> case second of",
      "                           0 -> 0",
      "                           k -> k",
      "                    x -> x + second",
      "              in t",
      "  in first + second + third"
    ]

expected :: Text
expected =
  T.init $
    T.unlines $
      [ "module IntegrationTest where",
        "",
        "f :: Int -> Int -> Int",
        "f x y = let a = case x of",
        "                  0 -> 1",
        "                  n -> let b = n + y",
        "                       in  case b of",
        "                             0 -> 0",
        "                             k -> k + a",
        "        in  a + y",
        "",
        "g :: [Int] -> Int",
        "g []     = 0",
        "g (x:xs) = let val  = case x of",
        "                       0 -> 10",
        "                       n -> n * 2",
        "               rest = case xs of",
        "                        [] -> 0",
        "                        ys -> let inner = case y of",
        "                                            _  -> length ys",
        "                              in  inner",
        "           in  val + rest",
        "",
        "h :: Int -> Int",
        "h x | x == 0    = 1",
        "    | x > 0     = let pos = case x of",
        "                              1 -> 1",
        "                              n -> n * 2",
        "                  in  pos",
        "    | otherwise = let neg = case x of",
        "                              _ -> (-x)",
        "                  in  neg",
        "",
        "complex :: Int -> Int -> Int -> Int",
        "complex a b c = let first = case a of",
        "                             0 -> let innerA = case b of",
        "                                                 0 -> 0",
        "                                                 n -> n + 1",
        "                                  in  innerA",
        "                             x -> x",
        "                    second = case (b, c) of",
        "                               (0, _) -> 0",
        "                               (_, 0) -> let innerB = case a of",
        "                                                        0 -> 1",
        "                                                        n -> n",
        "                                         in  innerB",
        "                               (m, n) -> m + n",
        "                    third = let t = case first of",
        "                                      0 -> case second of",
        "                                             0 -> 0",
        "                                             k -> k",
        "                                      x -> x + second",
        "                            in  t",
        "                in  first + second + third"
      ]
