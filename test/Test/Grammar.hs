-- | The grammar list, the built-in queries, and @him --grammar@'s fetch and
-- build (ADR grammar-setup). Fetching uses a repository in a temporary
-- directory as the remote, so nothing goes over the network; building
-- needs a C compiler and is skipped without one.
module Test.Grammar
  ( grammarTests
  , grammarIO
  ) where

import Control.Monad (when)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Him.Embedded (embeddedQueries)
import Him.GrammarBuild (buildGrammars, fetchGrammars)
import Him.GrammarList (GrammarSource (..), grammarList, parseGrammarList)
import Him.Language (Language (..), grammarFor, languages)
import Him.Process (ProcessResult (..), runProcess)
import System.Directory (createDirectoryIfMissing, doesFileExist, doesPathExist, findExecutable, getTemporaryDirectory, removeDirectoryRecursive)
import System.FilePath ((</>))
import Data.Text.Encoding qualified as TE
import Test.Harness

grammarTests :: [Test]
grammarTests =
  [ test "the built-in grammar list reads" (assertEqual (Right True) ((> 200) . length <$> grammarList))
  , test "a grammar in a subdirectory keeps its subpath" $
      assertEqual
        (Right [GrammarSource "tsx" "https://x/ts" "abc" (Just "tsx")])
        (parseGrammarList "[tsx]\ngit = \"https://x/ts\"\nrev = \"abc\"\nsubpath = \"tsx\"\n")
  , test "a grammar without a revision is an error" $
      assertEqual (Left "grammar x: no rev") (parseGrammarList "[x]\ngit = \"https://x\"\n")
  , test "every built-in language's grammar is in the list" $
      -- cabal has no tree-sitter grammar in Helix's list.
      assertEqual ["cabal"] [g | g <- builtinGrammars, g `notElem` either (const []) (map gsName) grammarList]
  , test "every built-in language has a query" $
      assertEqual ["cabal"] [langName l | l <- languages, not (hasQuery l)]
  , test "every inherited query is built in" $
      assertEqual [] [(lang, parent) | (lang, q) <- Map.toList embeddedQueries, parent <- inherits q, parent `Map.notMember` embeddedQueries]
  ]
  where
    builtinGrammars = mapMaybe (grammarFor "tree-sitter") languages
    hasQuery l = any (`Map.member` embeddedQueries) (langName l : mapMaybe (grammarFor "tree-sitter") [l])
    inherits q = [T.strip p | l <- T.lines q, Just ps <- [T.stripPrefix "; inherits:" (T.strip l)], p <- T.splitOn "," ps]

-- | Fetch from a local repository, then build a tiny grammar.
grammarIO :: IO [Test]
grammarIO = do
  tmp <- (</> "him-test-grammar") <$> getTemporaryDirectory
  exists <- doesPathExist tmp
  when exists (removeDirectoryRecursive tmp)
  let remote = tmp </> "remote"
      sources = tmp </> "runtime/grammars/sources"
      out = tmp </> "runtime/grammars"
      g dir args = runProcess "git" (["-c", "user.name=t", "-c", "user.email=t@t"] <> args) (Just dir) ""
      commit msg = do
        _ <- g remote ["add", "-A"]
        _ <- g remote ["commit", "-q", "-m", msg]
        either (const "") (T.strip . TE.decodeUtf8 . prStdout) <$> g remote ["rev-parse", "HEAD"]
      source rev = GrammarSource "tiny" (T.pack ("file://" <> remote)) rev (Just "grammar")
      quiet :: Text -> IO ()
      quiet _ = pure ()
  createDirectoryIfMissing True (remote </> "grammar/src")
  _ <- g remote ["init", "-q"]
  -- GitHub serves any commit by its hash; a local remote needs telling.
  _ <- g remote ["config", "uploadpack.allowAnySHA1InWant", "true"]
  writeFile (remote </> "grammar/src/parser.c") "void *tree_sitter_tiny(void) { return 0; }\n"
  rev1 <- commit "one"
  writeFile (remote </> "grammar/src/parser.c") "void *tree_sitter_tiny(void) { return (void *)0; }\n"
  rev2 <- commit "two"
  -- The first revision, as the list would pin it (not the remote's latest).
  fetched <- fetchGrammars sources [source rev1] quiet
  pinned <- either (const "") (T.strip . TE.decodeUtf8 . prStdout) <$> g (sources </> "tiny") ["rev-parse", "HEAD"]
  again <- fetchGrammars sources [source rev1] quiet
  moved <- fetchGrammars sources [source rev2] quiet
  missing <- fetchGrammars sources [GrammarSource "gone" (T.pack ("file://" <> tmp </> "nowhere")) rev1 Nothing] quiet
  cc <- findExecutable "cc"
  builds <- case cc of
    Nothing -> pure [test "no C compiler: building skipped" (Right ())]
    Just _ -> do
      built <- buildGrammars sources out False [source rev2] quiet
      so <- doesFileExist (out </> "tiny.so")
      upToDate <- buildGrammars sources out False [source rev2] quiet
      forced <- buildGrammars sources out True [source rev2] quiet
      unfetched <- buildGrammars sources out False [GrammarSource "other" "" "" Nothing] quiet
      pure
        [ test "a fetched grammar is built" (assertEqual ([("tiny", Right "built")], True) (built, so))
        , test "building it again from the same revision is skipped" (assertEqual [("tiny", Right "up to date")] upToDate)
        , test "--force builds it anyway" (assertEqual [("tiny", Right "built")] forced)
        , test "a grammar that was not fetched says so" $
            assertEqual [("other", Left "not fetched (him --grammar fetch)")] unfetched
        ]
  pure $
    [ test "a grammar is fetched at the pinned revision" (assertEqual ([("tiny", Right "fetched")], rev1) (fetched, pinned))
    , test "fetching it again does nothing" (assertEqual [("tiny", Right "up to date")] again)
    , test "a new revision in the list is fetched" (assertEqual [("tiny", Right "fetched")] moved)
    , test "a repository that is not there fails" (assertEqual [True] [either (const True) (const False) r | (_, r) <- missing])
    ]
      <> builds
