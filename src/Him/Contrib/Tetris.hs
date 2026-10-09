-- | A contrib plugin (ADR plugin-canvas): Tetris in a box in the middle of the
-- screen (@:tetris@). An example of a canvas (drawn cell by cell), a
-- keymap of its own while it is open, a timer (the pieces fall), and
-- keys the keymap leaves to the plugin ('CanvasKey': @r@ starts again).
--
-- Keys: @left@ / @right@ (or @h@ / @l@) move, @up@ / @k@ / @x@ rotate,
-- @z@ rotates back, @down@ / @j@ drop a line, @space@ drops the piece,
-- @p@ pauses, @q@ or @esc@ quits.
module Him.Contrib.Tetris
  ( tetris
    -- * The game (pure, for the tests)
  , Game (..)
  , newGame
  , cells
  , move
  , rotate
  , dropPiece
  , fall
  , boardWidth
  , boardHeight
  ) where

import Data.Bits (shiftR, xor)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Him.Plugin

boardWidth, boardHeight :: Int
boardWidth = 10
boardHeight = 20

-- | A game: the settled cells (by row and column, their piece), the
-- falling piece (kind, rotation, row and column of its box), the next
-- kinds, the random state, and the score.
data Game = Game
  { gBoard :: !(Map (Int, Int) Int)
  , gPiece :: !Int
  , gRotation :: !Int
  , gRow :: !Int
  , gCol :: !Int
  , gQueue :: ![Int]
  , gSeed :: !Word64
  , gScore :: !Int
  , gLines :: !Int
  , gOver :: !Bool
  , gPaused :: !Bool
  }
  deriving stock (Eq, Show)

tetris :: PluginSpec (Maybe Game)
tetris =
  (pluginSpec "tetris" "Tetris in a box in the middle of the screen (:tetris)" Nothing)
    { psDefaultOn = False
    , psActions =
        [ action "tetris" "Play Tetris" start
        , action "tetris_left" "Move the piece left" (play (move (-1)))
        , action "tetris_right" "Move the piece right" (play (move 1))
        , action "tetris_rotate" "Rotate the piece clockwise" (play (rotate 1))
        , action "tetris_rotate_back" "Rotate the piece anticlockwise" (play (rotate 3))
        , action "tetris_down" "Move the piece down a line" (play fall)
        , action "tetris_drop" "Drop the piece to the bottom" (play dropPiece)
        , action "tetris_pause" "Pause or go on" (update (\g -> if gOver g then g else g {gPaused = not (gPaused g)}))
        , action "tetris_quit" "Stop playing" quit
        ]
    , psCommands = [command ["tetris"] "Play Tetris" NoArgs (const start)]
    , psKeymaps =
        [ ( "game"
          ,
            [ ("left", "tetris_left")
            , ("h", "tetris_left")
            , ("right", "tetris_right")
            , ("l", "tetris_right")
            , ("up", "tetris_rotate")
            , ("k", "tetris_rotate")
            , ("x", "tetris_rotate")
            , ("z", "tetris_rotate_back")
            , ("down", "tetris_down")
            , ("j", "tetris_down")
            , ("space", "tetris_drop")
            , ("p", "tetris_pause")
            , ("q", "tetris_quit")
            ]
          )
        ]
    , psOnEvent = \case
        TimerFired "gravity" -> play fall
        -- Keys the keymap does not bind come here.
        CanvasKey "board" "r" -> getState >>= \g -> if maybe False gOver g then start else pure ()
        CanvasClosed "board" -> stopTimer "gravity" >> putState Nothing
        _ -> pure ()
    , psStop = stopTimer "gravity"
    }

start :: PluginM (Maybe Game) ()
start = do
  seed <- liftIO getMonotonicTimeNSec
  putState (Just (newGame seed))
  startTimer "gravity" (period 0)
  redraw

quit :: PluginM (Maybe Game) ()
quit = stopTimer "gravity" >> closeCanvas >> putState Nothing

-- | A move, while the game is on; a new level falls faster.
play :: (Game -> Game) -> PluginM (Maybe Game) ()
play f = getState >>= \case
  Just g | not (gOver g || gPaused g) -> do
    let g' = f g
    putState (Just g')
    if level g' /= level g then startTimer "gravity" (period (level g')) else pure ()
    if gOver g' then stopTimer "gravity" else pure ()
    redraw
  _ -> pure ()

update :: (Game -> Game) -> PluginM (Maybe Game) ()
update f = modifyState (fmap f) >> redraw

redraw :: PluginM (Maybe Game) ()
redraw = do
  -- The keys, as the game's keymap binds them (arrows as arrows).
  keys <- traverse (\(inv, what) -> (\k -> T.justifyLeft 5 ' ' (arrow k) <> what) <$> keyInKeymap "game" inv) helpKeys
  getState >>= mapM_ (\g -> showCanvas "board" (draw keys g) {canvasKeymap = Just "game"})
  where
    helpKeys = [("tetris_left", "left"), ("tetris_right", "right"), ("tetris_rotate", "rotate"), ("tetris_down", "down"), ("tetris_drop", "drop"), ("tetris_pause", "pause"), ("tetris_quit", "quit")]
    arrow = \case
      "left" -> "←"
      "right" -> "→"
      "up" -> "↑"
      "down" -> "↓"
      "space" -> "spc"
      k -> k

-- | Milliseconds between steps down at a level.
period :: Int -> Int
period lvl = max 80 (700 - 60 * lvl)

level :: Game -> Int
level g = gLines g `div` 10

-- * The rules

-- | The pieces (I O T S Z J L): their cells at rotation 0, and the size
-- of the box they turn in.
shapes :: [([(Int, Int)], Int)]
shapes =
  [ ([(1, 0), (1, 1), (1, 2), (1, 3)], 4)
  , ([(0, 0), (0, 1), (1, 0), (1, 1)], 2)
  , ([(0, 1), (1, 0), (1, 1), (1, 2)], 3)
  , ([(0, 1), (0, 2), (1, 0), (1, 1)], 3)
  , ([(0, 0), (0, 1), (1, 1), (1, 2)], 3)
  , ([(0, 0), (1, 0), (1, 1), (1, 2)], 3)
  , ([(0, 2), (1, 0), (1, 1), (1, 2)], 3)
  ]

-- | A piece's cells, turned clockwise so many times, in its box.
shape :: Int -> Int -> [(Int, Int)]
shape kind r = iterate (map turn) base !! (r `mod` 4)
  where
    (base, n) = shapes !! kind
    turn (row, col) = (col, n - 1 - row)

-- | Where the falling piece's cells are on the board.
cells :: Game -> [(Int, Int)]
cells g = [(gRow g + r, gCol g + c) | (r, c) <- shape (gPiece g) (gRotation g)]

fits :: Game -> Bool
fits g = all free (cells g)
  where
    free (r, c) = c >= 0 && c < boardWidth && r < boardHeight && Map.notMember (r, c) (gBoard g)

newGame :: Word64 -> Game
newGame seed = spawnPiece (Game Map.empty 0 0 0 0 [] (seed `xor` 0x9e3779b97f4a7c15) 0 0 False False)

-- | The next piece at the top (from a shuffled bag of all seven); the
-- game is over when it does not fit.
spawnPiece :: Game -> Game
spawnPiece g0 = g {gOver = not (fits g)}
  where
    (queue, seed) = if length (gQueue g0) < 2 then let (bag, s) = shuffle [0 .. 6] (gSeed g0) in (gQueue g0 <> bag, s) else (gQueue g0, gSeed g0)
    kind = head' queue
    g = g0 {gPiece = kind, gRotation = 0, gRow = 0, gCol = (boardWidth - snd (shapes !! kind)) `div` 2, gQueue = drop 1 queue, gSeed = seed}
    head' = \case
      k : _ -> k
      [] -> 0

move :: Int -> Game -> Game
move dc g = let g' = g {gCol = gCol g + dc} in if fits g' then g' else g

-- | Turn, nudging sideways off a wall or a stack if that makes it fit.
rotate :: Int -> Game -> Game
rotate turns g = case [g' | dc <- [0, -1, 1, -2, 2], let g' = g {gRotation = (gRotation g + turns) `mod` 4, gCol = gCol g + dc}, fits g'] of
  g' : _ -> g'
  [] -> g

-- | One line down, or settle the piece where it is.
fall :: Game -> Game
fall g = let g' = g {gRow = gRow g + 1} in if fits g' then g' else settle g

dropPiece :: Game -> Game
dropPiece g = let g' = g {gRow = gRow g + 1} in if fits g' then dropPiece g' {gScore = gScore g' + 2} else settle g

-- | The piece becomes part of the board; full rows go and score.
settle :: Game -> Game
settle g = spawnPiece g {gBoard = cleared, gLines = gLines g + n, gScore = gScore g + points * (level g + 1)}
  where
    board = foldl' (\b p -> Map.insert p (gPiece g) b) (gBoard g) (cells g)
    full = [r | r <- [0 .. boardHeight - 1], all (\c -> Map.member (r, c) board) [0 .. boardWidth - 1]]
    n = length full
    points = [0, 100, 300, 500, 800] !! min 4 n
    -- Rows above a full one move down past it.
    cleared = Map.fromList [((r + length (filter (> r) full), c), k) | ((r, c), k) <- Map.toList board, r `notElem` full]

-- | A shuffle from a small random generator (xorshift).
shuffle :: [Int] -> Word64 -> ([Int], Word64)
shuffle [] s = ([], s)
shuffle xs s =
  let s' = next s
      i = fromIntegral (s' `mod` fromIntegral (length xs))
   in case splitAt i xs of
        (before, x : after) -> let (rest, s'') = shuffle (before <> after) s' in (x : rest, s'')
        (before, []) -> (before, s')
  where
    next x0 = let x1 = x0 `xor` (x0 * 8192); x2 = x1 `xor` (x1 `shiftR` 7) in x2 `xor` (x2 * 131072)

-- * Drawing

faceOf :: Int -> Face
faceOf k = face (["function", "type", "keyword", "string", "diff.minus", "variable.builtin", "constant"] !! k)

-- | The board with its frame, and beside it the next piece, the score
-- and the keys.
draw :: [Text] -> Game -> Canvas
draw keys g = (canvas "tetris" (2 * boardWidth + 3 + 16) (boardHeight + 1)) {canvasRows = rows}
  where
    falling = Map.fromList [(p, gPiece g) | not (gOver g), p <- cells g]
    rowCells r = [maybe dot block (Map.lookup (r, c) falling <|> Map.lookup (r, c) (gBoard g)) | c <- [0 .. boardWidth - 1]]
    a <|> b = maybe b Just a
    block k = ("██", faceOf k)
    dot = (" ·", face "ui.virtual")
    frame = face "ui.window"
    message
      | gOver g = Just "GAME OVER"
      | gPaused g = Just "PAUSED"
      | otherwise = Nothing
    boardRow r = case message of
      Just m | r == boardHeight `div` 2 -> [(" │", frame), (T.center (2 * boardWidth) ' ' m, face "ui.text.focus"), ("│", frame)]
      _ -> [(" │", frame)] <> rowCells r <> [("│", frame)]
    nextShape = case gQueue g of
      k : _ -> [[if (r, c) `elem` shape k 0 then block k else ("  ", frame) | c <- [0 .. 3]] | r <- [0 .. 1]]
      [] -> []
    side =
      [ [label "next"]
      ]
        <> map (pad :) nextShape
        <> [ []
           , [label "score"]
           , [value (gScore g)]
           , [label "lines"]
           , [value (gLines g)]
           , [label "level"]
           , [value (level g)]
           , []
           ]
        <> map (\t -> [("  " <> t, face "comment")]) (if gOver g then ["r    again", "esc  quit"] else keys)
    pad = ("  ", frame)
    label t = ("  " <> t, face "ui.text.focus")
    value n = ("  " <> T.pack (show n), face "ui.text")
    rows = [boardRow r <> sideRow r | r <- [0 .. boardHeight - 1]] <> [[(" └" <> T.replicate (2 * boardWidth) "─" <> "┘", frame)]]
    sideRow r = case drop r side of
      s : _ -> s
      [] -> []
