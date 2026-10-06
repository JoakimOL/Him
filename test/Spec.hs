module Main (main) where

import Test.Harness
import Test.Text
import Test.Formats
import Test.Config
import Test.Git
import Test.Lsp
import Test.Syntax
import Test.Render
import Test.Integration
import Test.Windows
import Test.Match
import Test.Register
import Test.Jump
import Test.Chat
import Test.Repl

main :: IO ()
main = do
  integration <- integrationTests
  rebinding <- rebindTests
  processes <- processTests
  gitIO <- gitTests
  syntaxIO <- syntaxIOTests
  remapIO <- remapTests
  treeSitterIO <- treeSitterTests
  lspIO <- lspTests
  bufferIO <- openBufferTests
  loading <- loadingTests
  windows <- windowTests
  match <- matchTests
  registers <- registerTests
  jump <- jumpTests
  chat <- chatTests
  repl <- replTests
  runTests
    [ group "Him.Key" keyTests
    , group "Him.Terminal.Input.decodeKeys" decodeTests
    , group "Him.Buffer" bufferTests
    , group "Him.Buffer (randomized against a list model)" ropeModelTests
    , group "Him.Buffer.changeBetween" changeTests
    , group "Him.Motion.findChar" findCharTests
    , group "Him.Search (randomized against a naive search)" searchTests
    , group "Him.Motion" motionTests
    , group "Him.Edit" editTests
    , group "Him.History" historyTests
    , group "Him.Keymap" keymapTests
    , group "Him.Action" actionTests
    , group "Him.Picker" pickerTests
    , group "Him.Ignore" ignoreTests
    , group "Him.Json" jsonTests
    , group "Him.Diff" diffTests'
    , group "Him.GitState" gitStateTests
    , group "git (in a temporary repository)" gitIO
    , group "Him.Syntax" syntaxTests
    , group "Him.Regex" regexTests
    , group "Him.Lsp.Protocol" lspProtocolTests
    , group "Him.Lsp.Sync" syncTests
    , group "Him.Lsp.Edit" lspEditTests
    , group "Him.Toml" tomlTests
    , group "Him.UserConfig" userConfigTests
    , group "Him.Theme" themeTests
    , group "plugins" pluginTests
    , group "a remapping config file, through the keys" remapIO
    , group "LSP client (with clangd)" lspIO
    , group "highlighting through a provider" syntaxIO
    , group "Him.Syntax.TreeSitter (with the installed grammars)" treeSitterIO
    , group "Him.Process" processes
    , group "multiple selections" multiSelectionTests
    , group "Him.File" fileTests
    , group "Him.Ex" exTests
    , group "Him.View" viewTests
    , group "Him.TextWidth" widthTests
    , group "Him.Render" renderTests
    , group "Him.Render.Diff" diffTests
    , group "match mode (m) and I / A" match
    , group "registers and the clipboard (a fake one)" registers
    , group "jumplist" jump
    , group "AI chat (scripted provider; nothing live)" chat
    , group "windows (splits)" windows
    , group "REPL" repl
    , group "keys through the default config" integration
    , group "rebinding keys to actions" rebinding
    , group "buffers" bufferIO
    , group "Him.File (from disk)" loading
    ]
