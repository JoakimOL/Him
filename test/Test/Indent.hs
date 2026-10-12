-- | Indentation: detecting it, @<@ / @>@, and @.editorconfig@ files.
module Test.Indent
  ( indentTests
  ) where

import Control.Exception (IOException, try)
import Control.Monad.Trans.State.Strict (execStateT)
import Data.Foldable (foldlM)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Him.App (handleEvent)
import Him.Buffer qualified as B
import Him.Config.Default (defaultConfig)
import Him.Document
import Him.EditorConfig
import Him.Editor
import Him.Event (Event (..))
import Him.File (loadDocument)
import Him.Indent
import Him.Key (parseKeys)
import Him.Position (Pos (..))
import Him.Selection
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removeDirectoryRecursive)
import Test.Harness
import Test.Util

indentTests :: IO [Test]
indentTests = do
  config <- either (fail . T.unpack) pure defaultConfig
  let typeKeys ks ed = foldlM (\e k -> execStateT (handleEvent config (EvKey k)) e) ed (fromMaybe (error (show ks)) (parseKeys ks))
      after :: Document -> Text -> IO Text
      after d ks = B.toText . docBuffer . edDoc <$> typeKeys ks (newEditor (24, 80) d)
      plain t = newDocument Nothing (buf t)
  let twoSpaces d = d {docIndent = IndentHint (Just False) (Just 2)}
  indented <- after (twoSpaces (plain "a\n\nb")) "% >"
  twice <- after (twoSpaces (plain "a")) "2 >"
  outdented <- after ((plain "      a\n\tb\nc") {docIndent = IndentHint (Just False) (Just 4)}) "% <"
  tabbed <- after ((plain "a") {docIndent = IndentHint (Just True) Nothing}) "> i tab esc"
  -- A project: .editorconfig at the root, and a nested one for a directory.
  tmp <- getTemporaryDirectory
  let root = tmp <> "/him-test-editorconfig"
  _ <- try @IOException (removeDirectoryRecursive root)
  createDirectoryIfMissing True (root <> "/sub")
  writeFile (root <> "/.editorconfig") "root = true\n\n[*]\nindent_style = space\nindent_size = 3\n\n[*.{mk,go}]\nindent_style = tab\n"
  writeFile (root <> "/sub/.editorconfig") "[*.hs]\nindent_size = 8\n"
  writeFile (root <> "/a.txt") "x\n"
  writeFile (root <> "/b.go") "x\n"
  writeFile (root <> "/sub/c.hs") "x\n"
  writeFile (root <> "/sub/d.py") "def f():\n  return 1\n"
  loaded <- traverse (fmap (either (const noHint) docIndent) . loadDocument . (root <>)) ["/a.txt", "/b.go", "/sub/c.hs", "/sub/d.py", "/sub/new.hs"]
  removeDirectoryRecursive root
  pure
    [ test "detects tabs" (assertEqual (IndentHint (Just True) Nothing) (detectIndent (buf "a\n\tb\n\tc\n")))
    , test "detects the most common step of spaces" $
        assertEqual (IndentHint (Just False) (Just 4)) (detectIndent (buf "a\n    b\n        c\n    d\ne\n    g\n"))
    , test "too little indentation tells nothing" (assertEqual noHint (detectIndent (buf "a\nb\n  c\n")))
    , test "a hint is filled in from the next, then the settings" $
        assertEqual (Indent False 2) (resolveIndent [IndentHint Nothing (Just 2), IndentHint (Just False) (Just 8)] (Indent True 4))
    , test "indenting leaves the selection on the same text" $
        let (b, sel) = indentLines (Indent False 2) 1 (buf "ab\ncd") (single (Range (Pos 0 1) (Pos 1 0) Nothing))
         in assertEqual ("  ab\n  cd", [Range (Pos 0 3) (Pos 1 2) Nothing]) (B.toText b, ranges sel)
    , test "> indents every non-blank selected line" (assertEqual "  a\n\n  b" indented)
    , test "a count indents several levels" (assertEqual "    a" twice)
    , test "< takes a level of spaces or a tab off" (assertEqual "  a\nb\nc" outdented)
    , test "with tabs, > and tab in insert mode insert tabs" (assertEqual "\t\ta" tabbed)
    , test ".editorconfig: sections, braces, nesting, and the text after it" $
        assertEqual
          [ IndentHint (Just False) (Just 3)
          , IndentHint (Just True) (Just 3)
          , IndentHint (Just False) (Just 8)
          , IndentHint (Just False) (Just 3)
          , IndentHint (Just False) (Just 8)
          ]
          loaded
    , test "globs" $
        assertEqual
          [True, True, False, True, True, False, True, False]
          [ sectionMatches "*.hs" "src/A.hs"
          , sectionMatches "Makefile" "Makefile"
          , sectionMatches "/*.hs" "src/A.hs"
          , sectionMatches "src/**.hs" "src/x/A.hs"
          , sectionMatches "*.[ch]" "a.c"
          , sectionMatches "*.[!ch]" "a.c"
          , sectionMatches "{a,b/{c,d}}.txt" "b/d.txt"
          , sectionMatches "a?.txt" "a/.txt"
          ]
    , test "properties: later ones win, indent_size = tab takes tab_width" $
        assertEqual
          (IndentHint (Just True) (Just 6))
          (indentHint [("indent_style", "space"), ("indent_style", "tab"), ("indent_size", "tab"), ("tab_width", "6")])
    , test "parsing: root, comments, sections" $
        assertEqual
          (EditorConfig True [("*", [("indent_size", "2")])])
          (parseEditorConfig "# c\nroot = TRUE\n; c\n[*]\nIndent_Size = 2\n")
    ]
