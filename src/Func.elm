module Func exposing(..)

import Array exposing (..)
import Html exposing (..)
import Html.Attributes exposing (style, draggable)
import Html.Events exposing (on, onWithOptions, Options)
import Json.Decode as Json

import Types exposing(..)


--dropFields pieces drapPos =
--  let dragPieces = List.filter (\piece -> piece.pos == drapPos) pieces in
--  case (List.head dragPieces) of
--    Just dragPiece -> getDropFields dragPiece
--    _ -> []

--ドロップした際、駒の位置を更新
updatedPieces : List Piece -> Maybe Position -> Position -> List Piece
updatedPieces pieces dragPos dropPos=
  case dragPos of
      Just drag ->
        List.map (\piece ->
          if piece.pos == drag then
            {piece | pos=dropPos}
          else
            piece
          ) pieces

      _ -> pieces


yLow = 2
yHeg = 5
xLow = 0
xHeg = 3

rowLength = 8
colLength = 3

--駒を置くフィールド
fields : List Position
fields =
  Array.initialize (rowLength*colLength) (\i ->
    { x = i % colLength
    , y = i // colLength
    }
  ) |> Array.toList


-- フィールドの空いている位置リストを取得
getEmptyFields : List Piece -> List Position -> List Position
getEmptyFields pieces fields =
  let
    isEmpty pos pieces = List.isEmpty (List.filter (\piece -> piece.pos == pos) pieces)
  in
  List.filter (\pos -> isEmpty pos pieces) fields


-- ドロップできる位置リストを取得
getDropFields : Piece -> List Piece -> List Position
getDropFields piece pieces =
  let
    {x,y} = piece.pos
    p_type = piece.p_type
    isReserve = (y < 2 || y > 5 )
    forward x y = if (y-1 >= yLow) then [{x=x, y=(y-1)}] else []
    back x y = if (y+1 <= yHeg) then [{x=x, y=(y+1)}] else []
    left x y = if (x-1 >= xLow) then [{x=(x-1), y=y}] else []
    right x y = if (x+1 <= xHeg) then [{x=(x+1), y=y}] else []
    diagFL x y = if (x-1 >= xLow && y-1 >= yLow) then [{x=(x-1), y=(y-1)}] else []
    diagFR x y = if (x+1 <= xHeg && y-1 >= yLow) then [{x=(x+1), y=(y-1)}] else []
    diagBL x y = if (x-1 >= xLow && y+1 <= yHeg) then [{x=(x-1), y=(y+1)}] else []
    diagBR x y = if (x+1 <= xHeg && y+1 <= yHeg) then [{x=(x+1), y=(y+1)}] else []
  in
  if isReserve then
    getEmptyFields pieces fields
  else
    case p_type of
    LION ->
      forward x y ++ back x y ++ left x y ++ right x y ++ diagFL x y ++ diagFR x y ++ diagBL x y ++ diagBR x y ++ []
    ELEP ->
      diagFL x y ++ diagFR x y ++ diagBL x y ++ diagBR x y ++ []
    GIRA ->
      forward x y ++ back x y ++ left x y ++ right x y ++ []
    CHICK ->
      forward x y ++ []
    CHICKEN ->
      forward x y ++ back x y ++ left x y ++ right x y ++ diagFL x y ++ diagFR x y ++ []
    _ -> []

onDrop : msg -> Attribute msg
onDrop msg =
  on "drop" (Json.succeed msg)

onDragStart : msg -> Attribute msg
onDragStart msg =
  on "dragstart" (Json.succeed msg)

onDragEnd : msg -> Attribute msg
onDragEnd msg =
  on "dragend" (Json.succeed msg)

onDragEnter : msg -> Attribute msg
onDragEnter msg =
  on "dragenter" (Json.succeed msg)

dragOverPrevent : msg -> Attribute msg
dragOverPrevent msg =
  onWithOptions "dragover" {stopPropagation = False, preventDefault = True} (Json.succeed msg)
