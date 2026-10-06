module Main (main) where

import Him.Main (himMain)

-- | The released @him@: the built-in plugins and contrib (ADR personal-builds).
main :: IO ()
main = himMain []
