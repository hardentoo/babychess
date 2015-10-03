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
  turn :: PieceColor,
  board :: Board
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
        evaluateSquare (Square (Nothing)) = 0
        evaluateSquare (Square (Just piece)) = valuePiece piece

xx = Square Nothing
black p = Square $ Just $ Piece (Black, p)
white p = Square $ Just $ Piece (White, p)

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

initialBoard = Board
  [[br,bn,bb,bk,bq,bb,bn,br]
  ,[bp,bp,bp,bp,bp,bp,bp,bp]
  ,[xx,xx,xx,xx,xx,xx,xx,xx]
  ,[xx,xx,xx,xx,xx,xx,xx,xx]
  ,[xx,xx,xx,xx,xx,xx,xx,xx]
  ,[xx,xx,xx,xx,xx,xx,xx,xx]
  ,[wp,wp,wp,wp,wp,wp,wp,wp]
  ,[wr,wn,wb,wk,wq,wb,wn,wr]
  ]

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

printBoard b = do
  putStr $ prettyBoard b
  putStrLn $ "evaluation: " ++ show (evaluateBoard b)

main = do 
  putStrLn "babychess 0.0\n"
  printBoard initialBoard
  putStrLn ""
  printBoard whiteWinningBoard
  putStrLn ""
