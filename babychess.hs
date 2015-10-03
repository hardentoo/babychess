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
  | Bishop
  | Knight
  | Rook
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
prettyType Bishop = "B"
prettyType Knight = "N"
prettyType Rook = "R"
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

main = do 
putStrLn "babychess 0.0"
putStr $ prettyBoard initialBoard
