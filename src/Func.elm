module Func exposing(..)

import Array exposing (..)
import Html exposing (..)
import Html.Attributes exposing (style, draggable)
import Html.Events exposing (on, onWithOptions, Options)
import Json.Decode as Json

import Types exposing(..)

--ドロップした際、駒の位置を更新
updatedPieces : Bool -> List Piece -> Maybe Position -> Position -> List Piece
updatedPieces turn pieces dragPos dropPos=
  case dragPos of
      Just drag ->
        let
          owner = if turn then MY else ENEMY
          reservPos = getEmptyReservePos turn pieces
          pieces = case (getEmptyReservePos turn pieces) of
            Just resPos ->
              List.map (\piece ->
                             if (piece.pos == dropPos && not (piece.own == owner)) then
                               {piece | pos=resPos, own=owner}
                             else
                               piece
                             ) pieces
            Nothing -> pieces
        in
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

-- 敵持ち駒フィールド
enemyReserveFields : List Position
enemyReserveFields = Array.initialize (yLow*colLength) (\i ->
    { x = i % colLength
    , y = i // colLength
    }) |> Array.toList

-- 自分持ち駒フィールド
myReserveFields : List Position
myReserveFields = Array.initialize (yLow*colLength) (\i ->
    { x = i % colLength
    , y = (i // colLength) + (yHeg + 1)
    }) |> Array.toList

-- 持ち駒フィールドの空いているポジション取得
getEmptyReservePos : Bool -> List Piece -> Maybe Position
getEmptyReservePos myTurn pieces =
  let
    isFieldEmpty pos pieces = List.isEmpty (List.filter (\piece -> piece.pos == pos) pieces)
    emptyReservePos reserveFields = List.filter (\pos -> isFieldEmpty pos pieces) reserveFields
  in
  if(myTurn) then
    (List.head (emptyReservePos myReserveFields))
  else
    (List.head (emptyReservePos enemyReserveFields))


-- フィールドの空いている位置リストを取得
getEmptyFields : List Piece -> List Position -> List Position
getEmptyFields pieces fields =
  let
    isEmpty pos pieces = List.isEmpty (List.filter (\piece -> piece.pos == pos) pieces)
  in
  List.filter (\pos -> isEmpty pos pieces) fields


-- ドロップできる位置リストを取得
getDropFields : Piece -> Model -> List Position
getDropFields piece model =
  let
    _ = Debug.log "d:" piece.pos
    _ = Debug.log "d:" piece.own
    pieces = model.pieces
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
    owner = if model.turn then MY else ENEMY
  in
  if (not ( (piece.own == MY && model.turn) || (piece.own == ENEMY && not model.turn) )) then
    []
  else if isReserve then
    getEmptyFields pieces fields
  else
    let myPiecePoses = List.map (\piece -> piece.pos) (List.filter (\piece -> piece.own == owner) pieces) in
    case p_type of
    LION ->
      let movePoses = forward x y ++ back x y ++ left x y ++ right x y ++ diagFL x y ++ diagFR x y ++ diagBL x y ++ diagBR x y ++ [] in
      List.filter (\pos -> not (List.member pos myPiecePoses)) movePoses
    ELEP ->
      let movePoses = diagFL x y ++ diagFR x y ++ diagBL x y ++ diagBR x y ++ [] in
      List.filter (\pos -> not (List.member pos myPiecePoses)) movePoses
    GIRA ->
      let movePoses = forward x y ++ back x y ++ left x y ++ right x y ++ [] in
      List.filter (\pos -> not (List.member pos myPiecePoses)) movePoses
    CHICK ->
      if (model.turn) then
        let movePoses = forward x y ++ [] in
        List.filter (\pos -> not (List.member pos myPiecePoses)) movePoses
      else
        let movePoses = back x y ++ [] in
        List.filter (\pos -> not (List.member pos myPiecePoses)) movePoses
    CHICKEN ->
      if (model.turn) then
        let movePoses = forward x y ++ back x y ++ left x y ++ right x y ++ diagFL x y ++ diagFR x y ++ [] in
        List.filter (\pos -> not (List.member pos myPiecePoses)) movePoses
      else
        let movePoses = forward x y ++ back x y ++ left x y ++ right x y ++ diagBL x y ++ diagBR x y ++ [] in
        List.filter (\pos -> not (List.member pos myPiecePoses)) movePoses
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
