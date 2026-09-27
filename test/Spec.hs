module Main (main) where

import Test.Harness

main :: IO ()
main =
  runTests
    [ group
        "harness"
        [ test "assertEqual accepts equal values" (assertEqual (1 :: Int) 1)
        ]
    ]
