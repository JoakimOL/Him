-- | Highlighting: the provider interface and tree-sitter.
module Test.Syntax
  ( syntaxTests
  , syntaxIOTests
  , treeSitterTests
  ) where

import Control.Monad.Trans.State.Strict (execStateT)
import Data.Foldable (foldlM)
import Data.Maybe (fromMaybe)
import Him.App (handleEvent)
import Him.Buffer qualified as B
import Him.Config (Config (..))
import Him.Config.Default (defaultConfig)
import Him.Document
import Him.Editor
import Him.Event (Event (..))
import Him.Syntax
import Him.Syntax.TreeSitter (findQuery, findRuntime, readQuery, treeSitter)
import Data.List (find)
import Data.Maybe (isJust)
import Him.Language (detectLanguage, langName, languages)
import Data.IntMap.Strict qualified as IntMap
import Him.Key
import Him.Terminal.Ansi (packStyle)
import Him.Render (render)
import Him.Render.Frame (Cell (..), Frame (..))
import Him.Render.Theme (defaultTheme, scopeStyle)
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Test.Harness
import Test.Util

syntaxTests :: [Test]
syntaxTests =
  [ test "flatten: the earlier span wins an overlap" $
      assertEqual [LineSpan 0 4 "a", LineSpan 4 6 "b"] (flatten [LineSpan 0 4 "a", LineSpan 2 6 "b"])
  , test "flatten: a later span is cut around an earlier one inside it" $
      assertEqual [LineSpan 0 2 "outer", LineSpan 2 4 "inner", LineSpan 4 8 "outer"] (flatten [LineSpan 2 4 "inner", LineSpan 0 8 "outer"])
  , test "flatten: adjacent spans of one scope merge" (assertEqual [LineSpan 0 6 "k"] (flatten [LineSpan 0 3 "k", LineSpan 3 6 "k"]))
  , test "languages by extension, file name and shebang" $
      assertEqual [Just "rust", Just "make", Just "python", Just "bash", Nothing]
        [ langName <$> detectLanguage languages "src/main.rs" ""
        , langName <$> detectLanguage languages "dir/Makefile" ""
        , langName <$> detectLanguage languages "script" "#!/usr/bin/env python3"
        , langName <$> detectLanguage languages "run" "#!/bin/bash -e"
        , langName <$> detectLanguage languages "notes.xyz" "hello"
        ]
  , test "scopes resolve by their longest known prefix" $
      assertEqual
        [scopeStyle defaultTheme "keyword.control", scopeStyle defaultTheme "comment", Nothing]
        [scopeStyle defaultTheme "keyword.control.import", scopeStyle defaultTheme "comment.line.double-slash", scopeStyle defaultTheme "nothing.like.this"]
  ]

-- | Highlighting through an injected provider.
syntaxIOTests :: IO [Test]
syntaxIOTests = do
  base <- either (fail . T.unpack) pure defaultConfig
  let decline = SyntaxProvider "declines" (const (pure Nothing))
      config = base {cfgSyntaxProviders = [decline, fakeProvider]}
      start path t = execStateT (handleEvent config (EvResize 10 40)) (newEditor (10, 40) (newDocument (Just path) (buf t)))
      run ed k = execStateT (handleEvent config (EvKey k)) ed
      keys ks ed = foldlM run ed (fromMaybe (error ks) (parseKeys (T.pack ks)))
      syntaxOf ed = docSyntax (edDoc ed)
  rt <- testRuntime config
  opened <- settleWith config rt =<< start "a.hs" "let a = 1\nin a"
  edited <- settleWith config rt =<< keys "i l e t space esc" opened
  plainFile <- settle config =<< start "a.txt" "let a = 1"
  let frame = render defaultTheme Nothing opened
      cellStyleAt row col = fmap cellStyle (Seq.lookup col =<< Seq.lookup row (frameCells frame))
  pure
    [ test "the first provider that accepts the language is used" (assertEqual (SyntaxActive "fake") (siStatus (syntaxOf opened)))
    , test "spans arrive for the visible lines" (assertEqual (Just [LineSpan 0 3 "keyword"], Just []) (IntMap.lookup 0 (siSpans (syntaxOf opened)), IntMap.lookup 1 (siSpans (syntaxOf opened))))
    , test "an edit is highlighted again" (assertEqual (Just [LineSpan 0 3 "keyword", LineSpan 4 7 "keyword"]) (IntMap.lookup 0 (siSpans (syntaxOf edited))))
    , test "a file without a language is not highlighted" (assertEqual SyntaxNone (siStatus (syntaxOf plainFile)))
    , test "spans are drawn in their scope's style" $
        -- Column 6: past the gutter and the cursor cell, inside "let".
        assertEqual (packStyle <$> scopeStyle defaultTheme "keyword") (cellStyleAt 0 6)
    ]

-- | The tree-sitter provider on real grammars, when a runtime directory
-- with the Rust grammar exists (otherwise the tests pass, saying so).
treeSitterTests :: IO [Test]
treeSitterTests =
  findRuntime "rust" >>= \case
    Nothing -> pure [test "tree-sitter runtime not found: skipped" (Right ())]
    Just _ -> do
      let rust = fromMaybe (error "rust") (find ((== "rust") . langName) languages)
          source = B.fromText "// note\nfn main() {\n    let x = \"hi\\n\";\n}"
      session <- spStart treeSitter rust
      spans <- case session of
        Nothing -> pure IntMap.empty
        Just s -> ssUpdate s 1 source [] >> ssHighlight s 0 3
      -- The built-in queries (no runtime directory overrides them).
      inherited <- readQuery (findQuery []) "typescript"
      let scopesOn l = [lsScope sp | sp <- IntMap.findWithDefault [] l spans]
      pure
        [ test "a Rust grammar loads" (assertEqual True (isJust session))
        , test "a comment" (assertEqual ["comment.line"] (map (T.take 12) (scopesOn 0)))
        , test "keywords, functions and strings" $
            assertEqual (True, True, True)
              ( any ("keyword" `T.isPrefixOf`) (scopesOn 1)
              , any ("function" `T.isPrefixOf`) (scopesOn 1)
              , any ("string" `T.isPrefixOf`) (scopesOn 2)
              )
        , test "an escape inside a string wins over the string" $
            assertEqual True (any ("constant.character.escape" `T.isPrefixOf`) (scopesOn 2))
        , test "spans are sorted and do not overlap" $
            assertEqual True (and [lsEnd a <= lsStart b | l <- [0 .. 3], let ss = IntMap.findWithDefault [] l spans, (a, b) <- zip ss (drop 1 ss)])
        , test "inherited queries are read in place of the inherits line" $
            assertEqual (Just False) (T.isInfixOf "; inherits" <$> inherited)
        ]
