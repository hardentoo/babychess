import Data.Char

newtype Board = Board [[Square]]
  deriving (Show, Eq)

newtype Square = Square (Maybe Piece)
  deriving (Show, Eq)

newtype Pos = Pos (Int, Int)
  deriving (Show, Eq)

data PieceType 
  = King
  | Queen
  | Rook
  | Bishop
  | Knight
  | Pawn
  deriving (Show, Eq)

data PieceColor = Black | White
  deriving (Show, Eq)

newtype Piece = Piece (PieceColor, PieceType)
  deriving (Show, Eq)

data State = State {
  stateTurn :: PieceColor,
  stateBoard :: Board
}

prettyType :: PieceType -> String
prettyType King = "K"
prettyType Queen = "Q"
prettyType Rook = "R"
prettyType Bishop = "B"
prettyType Knight = "N"
prettyType Pawn = "P"

prettyPiece :: Piece -> String
prettyPiece (Piece (Black, typ)) = map toLower $ prettyType typ
prettyPiece (Piece (White, typ)) = prettyType typ

prettySquare :: Square -> String
prettySquare (Square Nothing) = "."
prettySquare (Square (Just p)) = prettyPiece p

prettyBoard :: Board -> String
prettyBoard (Board board) = unlines (map prettyRow board)
  where prettyRow = concatMap prettySquare

valuePieceType :: PieceType -> Int
valuePieceType King = 1000
valuePieceType Queen = 9
valuePieceType Rook = 5
valuePieceType Bishop = 3
valuePieceType Knight = 3
valuePieceType Pawn = 1

valuePiece :: Piece -> Int
valuePiece (Piece (Black, typ)) = negate $ valuePieceType typ
valuePiece (Piece (White, typ)) = valuePieceType typ

evaluateBoard :: Board -> Int
evaluateBoard (Board rows) = sum $ map evaluateRow rows
  where evaluateRow row = sum $ map evaluateSquare row
        evaluateSquare (Square Nothing) = 0
        evaluateSquare (Square (Just piece)) = valuePiece piece

--
-- FIX moving is very inefficient. Use vectors?
--

updateItem :: [x] -> x -> Int -> [x]
updateItem [] _ _ = []
updateItem (x:xs) y index = if index == 0 
                            then y:xs
                            else x:updateItem xs y (index - 1)

updateSquare :: Board -> Square -> Pos -> Board
updateSquare (Board squares) s (Pos (x, y)) = Board $ updateItem squares newRow y
  where newRow = updateItem (squares !! y) s x

squareAt :: Board -> Pos -> Square
squareAt (Board squares) (Pos (x, y)) = squares !! y !! x

movePos :: Pos -> Pos -> Board -> Board
movePos oldPos newPos board = updateSquare board1 (squareAt board oldPos) newPos
 where board1 = updateSquare board (Square Nothing) oldPos

black :: PieceType -> Square
black p = Square $ Just $ Piece (Black, p)

white :: PieceType -> Square
white p = Square $ Just $ Piece (White, p)

xx,bk,bq,bb,bn,br,bp,wk,wq,wb,wn,wr,wp :: Square

xx = Square Nothing
bk = black King
bq = black Queen
bb = black Bishop
bn = black Knight
br = black Rook
bp = black Pawn

wk = white King
wq = white Queen
wb = white Bishop
wn = white Knight
wr = white Rook
wp = white Pawn

initialBoard :: Board
initialBoard = Board
  [[br,bn,bb,bq,bk,bb,bn,br]
  ,[bp,bp,bp,bp,bp,bp,bp,bp]
  ,[xx,xx,xx,xx,xx,xx,xx,xx]
  ,[xx,xx,xx,xx,xx,xx,xx,xx]
  ,[xx,xx,xx,xx,xx,xx,xx,xx]
  ,[xx,xx,xx,xx,xx,xx,xx,xx]
  ,[wp,wp,wp,wp,wp,wp,wp,wp]
  ,[wr,wn,wb,wq,wk,wb,wn,wr]
  ]

-- e2-e4
move1 :: Board
move1 = movePos (Pos (4,6)) (Pos (4, 4)) initialBoard

whiteWinningBoard :: Board
whiteWinningBoard = Board
  [[bk,xx,xx,xx,xx,xx,xx,xx]
  ,[bp,bp,xx,xx,xx,xx,xx,xx]
  ,[xx,xx,xx,xx,xx,xx,xx,xx]
  ,[xx,xx,xx,xx,xx,xx,xx,xx]
  ,[xx,xx,xx,xx,xx,xx,xx,xx]
  ,[xx,xx,xx,xx,xx,xx,xx,xx]
  ,[wp,wp,wp,xx,xx,xx,xx,xx]
  ,[xx,wk,xx,wr,xx,xx,xx,xx]
  ]

printBoard :: String -> Board -> IO ()
printBoard name b = do
  putStrLn ""
  putStrLn name
  putStr $ prettyBoard b
  putStrLn $ "evaluation: " ++ show (evaluateBoard b)

main :: IO ()
main =
  putStrLn "babychess 0.0\n" >>
  printBoard "initial" initialBoard >>
  printBoard "e2-e4" move1 >>
  printBoard "white winning" whiteWinningBoard
