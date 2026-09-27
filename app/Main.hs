module Main (main) where

import Him.App qualified as App
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main =
  getArgs >>= \case
    [] -> App.run Nothing
    [file] -> App.run (Just file)
    _ -> hPutStrLn stderr "usage: him [FILE]" >> exitFailure
