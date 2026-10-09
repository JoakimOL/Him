-- | The contrib collection (ADR plugin-api): plugins built on "Him.Plugin" only,
-- compiled into every release and off until switched on (@[plugins]@ or
-- @:plugin-enable@). To add one: a module under "Him.Contrib", its line
-- here, its tests, and its part in the tutorial.
module Him.Contrib
  ( contribPlugins
  ) where

import Him.Config (Plugin)
import Him.Contrib.Magit (magit)
import Him.Contrib.RecentFiles (recentFiles)
import Him.Contrib.Tetris (tetris)
import Him.Contrib.WordCount (wordCount)
import Him.Plugin.Host (hostPlugin)

contribPlugins :: [Plugin]
contribPlugins =
  [ hostPlugin wordCount
  , hostPlugin recentFiles
  , hostPlugin magit
  , hostPlugin tetris
  ]
