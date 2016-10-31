module Func exposing(..)

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

--ドロップした際、駒を位置を更新
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

-- ドロップできる位置リストを取得
getDropFields : Piece -> List Position
getDropFields piece =
  let
    {x,y} = piece.pos
    p_type = piece.p_type
    forward x y = if (y-1 >= 0) then [{x=x, y=(y-1)}] else []
    back x y = if (y+1 <= 3) then [{x=x, y=(y+1)}] else []
    left x y = if (x-1 >= 0) then [{x=(x-1), y=y}] else []
    right x y = if (x+1 <= 2) then [{x=(x+1), y=y}] else []
    diagFL x y = if (x-1 >= 0 && y-1 >= 0) then [{x=(x-1), y=(y-1)}] else []
    diagFR x y = if (x+1 <= 2 && y-1 >= 0) then [{x=(x+1), y=(y-1)}] else []
    diagBL x y = if (x-1 >= 0 && y+1 <= 3) then [{x=(x-1), y=(y+1)}] else []
    diagBR x y = if (x+1 <= 3 && y+1 <= 3) then [{x=(x+1), y=(y+1)}] else []
  in
  -- yが縦(max,3)、xが横(max,2)
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
