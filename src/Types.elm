module Types exposing(Model, Msg(..), Own(..), Type(..), Position, Piece)

type Type = LION | ELEP | GIRA | CHICK | CHICKEN | NoAnimal
type Own = MY | ENEMY

type alias Position = {x:Int, y:Int}
type alias Piece = {own:Own, p_type:Type, pos:Position}

type alias Model = {
  turn:Own
  , drag: Maybe Position
  , isDropFields: List Position
  , pieces:List Piece
  , promotePos: Maybe Position
  }

type Msg
  = DragStart Position
  | DragEnd
  | DragEnter Position
  | Drop Position
  | NoPromote
  | Promoted Position
  | NoOp


