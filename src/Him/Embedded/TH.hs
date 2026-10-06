{-# LANGUAGE TemplateHaskell #-}

-- | Splices that put files of @runtime/@ into the binary at compile time
-- (used by "Him.Embedded"; the stage restriction keeps them apart). Paths
-- are relative to the package, which is where GHC runs. Each file is a
-- dependency of the module, so editing one rebuilds it; a new directory
-- needs the module touched (@dev/sync-helix-runtime.py@ does that).
module Him.Embedded.TH
  ( embedText
  , embedQueries
  ) where

import Control.Monad (filterM)
import Data.ByteString qualified as BS
import Data.List (sort)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient)
import Language.Haskell.TH (Exp, Q, listE, litE, runIO, stringL, tupE)
import Language.Haskell.TH.Syntax (addDependentFile)
import System.Directory (doesFileExist, listDirectory)
import System.FilePath ((</>))

-- | A UTF-8 file as a 'String' literal.
embedText :: FilePath -> Q Exp
embedText path = do
  addDependentFile path
  litE . stringL =<< runIO (readUtf8 path)

-- | @[(language, highlights.scm)]@ for each @DIR/LANG/highlights.scm@, as
-- 'String' literals.
embedQueries :: FilePath -> Q Exp
embedQueries dir = do
  langs <- runIO (sort <$> (filterM (doesFileExist . file) =<< listDirectory dir))
  mapM_ (addDependentFile . file) langs
  listE [tupE [litE (stringL lang), litE . stringL =<< runIO (readUtf8 (file lang))] | lang <- langs]
  where
    file lang = dir </> lang </> "highlights.scm"

readUtf8 :: FilePath -> IO String
readUtf8 path = T.unpack . decodeUtf8Lenient <$> BS.readFile path
