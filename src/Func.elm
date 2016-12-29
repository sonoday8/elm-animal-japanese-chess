module Func exposing(..)

import Array exposing (..)
import Html exposing (..)
import Html.Attributes exposing (style, draggable)
import Html.Events exposing (on, onWithOptions, Options)
import Json.Decode as Json

import Types exposing(..)

--ドロップした際、駒の位置を更新
updatedPieces : Position -> Model -> List Piece
updatedPieces dropPos model =
  let
    owner = model.turn
    pieces = model.pieces
    maybeDragPos = model.drag
  in
  case maybeDragPos of
      Just dragPos ->
        let
          -- ドロップ時に駒があれば持ち駒として取得
          reservePiece : Piece -> Piece
          reservePiece piece =
            case (getMaybeEmptyReservePos owner pieces) of
               Just reservePos ->
                 if (piece.pos == dropPos && not (piece.own == owner)) then
                   {piece | pos=reservePos, own=owner}
                 else
                   piece
               Nothing -> piece
          -- 持ち駒取得処理
          pieces : List Piece
          pieces = List.map (\piece -> reservePiece piece) pieces
          -- 駒をドロップ
          dropPiece : Piece -> Piece
          dropPiece piece =
            if piece.pos == dragPos then
              {piece | pos=dropPos}
            else
              piece
        in
        List.map (\piece -> dropPiece piece ) pieces
      _ -> pieces

 -- ポジションにある駒を取得
maybeGetPiece : Position -> List Piece -> Maybe Piece
maybeGetPiece dragPos pieces = List.filter (\piece -> piece.pos == dragPos) pieces |> List.head

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
getMaybeEmptyReservePos : Own -> List Piece -> Maybe Position
getMaybeEmptyReservePos owner pieces =
  let
    isFieldEmpty pos pieces = List.isEmpty (List.filter (\piece -> piece.pos == pos) pieces)
    emptyReservePos reserveFields = List.filter (\pos -> isFieldEmpty pos pieces) reserveFields
  in
  if(owner == MY) then
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
  in
  if (not ( piece.own == model.turn ) ) then
    []
  else if isReserve then
    getEmptyFields pieces fields
  else
    let myPiecePoses = List.map (\piece -> piece.pos) (List.filter (\piece -> piece.own == model.turn) pieces) in
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
      if (model.turn == MY) then
        let movePoses = forward x y ++ [] in
        List.filter (\pos -> not (List.member pos myPiecePoses)) movePoses
      else
        let movePoses = back x y ++ [] in
        List.filter (\pos -> not (List.member pos myPiecePoses)) movePoses
    CHICKEN ->
      if (model.turn == MY) then
        let movePoses = forward x y ++ back x y ++ left x y ++ right x y ++ diagFL x y ++ diagFR x y ++ [] in
        List.filter (\pos -> not (List.member pos myPiecePoses)) movePoses
      else
        let movePoses = forward x y ++ back x y ++ left x y ++ right x y ++ diagBL x y ++ diagBR x y ++ [] in
        List.filter (\pos -> not (List.member pos myPiecePoses)) movePoses
    _ -> []

-- ターン交代
changeTurn owner = if owner == MY then ENEMY else MY

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
