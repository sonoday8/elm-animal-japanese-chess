module Types exposing(..)

type Type = LION | ELEP | GIRA | CHICK | CHICKEN | NoAnimal
type Own = MY | ENEMY

type alias Position = {x:Int, y:Int}
type alias Piece = {own:Own, p_type:Type, isDrag:Bool, pos:Position}
