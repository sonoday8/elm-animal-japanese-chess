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
                   if piece.p_type == CHICKEN then
                     {piece | p_type=CHICK, pos=reservePos, own=owner}
                   else
                     {piece | pos=reservePos, own=owner}
                 else
                   piece
               Nothing -> piece
          -- 持ち駒取得処理
          pieces_ : List Piece
          pieces_ = List.map (\piece -> reservePiece piece) pieces
          -- 駒をドロップ
          dropPiece : Piece -> Piece
          dropPiece piece =
            if piece.pos == dragPos then
              {piece | pos=dropPos}
            else
              piece
        in
        List.map (\piece -> dropPiece piece ) pieces_
      _ -> pieces

 -- ポジションにある駒を取得
maybeGetPiece : Position -> List Piece -> Maybe Piece
maybeGetPiece pos pieces = List.filter (\piece -> piece.pos == pos) pieces |> List.head

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

-- そのポジションは持ち駒フィールドかどうか
isReservePos : Position -> Bool
isReservePos pos = (pos.y < 2 || pos.y > 5 )

-- そのポジションは敵陣地かどうか
isEnemyFieldPos : Position -> Own -> Bool
isEnemyFieldPos pos owner =
  if owner == MY then (2 <= pos.y && pos.y <= 3)
  else (4 <= pos.y && pos.y <= 5)

-- ドロップできる位置リストを取得
getDropFields : Piece -> Model -> List Position
getDropFields piece model =
  let
    pieces = model.pieces
  in
  if (not ( piece.own == model.turn ) ) then
    []
  else if isReservePos piece.pos then
    getEmptyFields pieces fields
  else
    let
      myPiecePoses = List.map (\piece -> piece.pos) (List.filter (\piece -> piece.own == model.turn) pieces)
      movePoses_ = movePoses model.turn piece
    in
    List.filter (\pos -> not (List.member pos myPiecePoses)) movePoses_

{-|

-}
movePoses : Own -> Piece -> List Position
movePoses turn piece =
  let
    {x, y} = piece.pos
    forward x y = if (y-1 >= yLow) then [{x=x, y=(y-1)}] else []
    back x y = if (y+1 <= yHeg) then [{x=x, y=(y+1)}] else []
    left x y = if (x-1 >= xLow) then [{x=(x-1), y=y}] else []
    right x y = if (x+1 <= xHeg) then [{x=(x+1), y=y}] else []
    diagFL x y = if (x-1 >= xLow && y-1 >= yLow) then [{x=(x-1), y=(y-1)}] else []
    diagFR x y = if (x+1 <= xHeg && y-1 >= yLow) then [{x=(x+1), y=(y-1)}] else []
    diagBL x y = if (x-1 >= xLow && y+1 <= yHeg) then [{x=(x-1), y=(y+1)}] else []
    diagBR x y = if (x+1 <= xHeg && y+1 <= yHeg) then [{x=(x+1), y=(y+1)}] else []
  in
  case piece.p_type of
    LION ->
      forward x y ++ back x y ++ left x y ++ right x y ++ diagFL x y ++ diagFR x y ++ diagBL x y ++ diagBR x y ++ []
    ELEP ->
      diagFL x y ++ diagFR x y ++ diagBL x y ++ diagBR x y ++ []
    GIRA ->
      forward x y ++ back x y ++ left x y ++ right x y ++ []
    CHICK ->
      if (turn == MY) then
        forward x y ++ []
      else
        back x y ++ []
    CHICKEN ->
      if (turn == MY) then
        forward x y ++ back x y ++ left x y ++ right x y ++ diagFL x y ++ diagFR x y ++ []
      else
        forward x y ++ back x y ++ left x y ++ right x y ++ diagBL x y ++ diagBR x y ++ []
    _ -> []

-- ターン交代
changeTurn : Own -> Own
changeTurn owner = if owner == MY then ENEMY else MY

-- Justかどうか
isJust : Maybe a -> Bool
isJust a =
  case a of
    Just a -> True
    Nothing -> False

--駒なり処理
promotePieces : Maybe Position -> List Piece -> List Piece
promotePieces maybePos pieces =
  let
    updateCHICKEN piece =
      if piece.p_type == CHICK then
        case maybePos of
          Just pos ->
            if (piece.pos == pos) then
              {piece | p_type=CHICKEN}
            else
              piece
          Nothing -> piece
      else
        piece
  in
  List.map (\piece -> updateCHICKEN piece ) pieces

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


enemyLogic pieces =
  let
    myPieces = List.filter (\piece -> piece.own == MY) pieces
    myCanMvPlaces = List.map (\piece -> movePoses MY piece) myPieces |> List.concat
    enemyPieces = List.filter (\piece -> piece.own == ENEMY) pieces
    enemyCanMvPlaces = List.map (\piece -> movePoses ENEMY piece) enemyPieces |> List.concat

    --そのポジションに移動できる相手駒
    myCanMvPieces pos = List.filter (\piece ->
        List.member pos (movePoses MY piece)
      ) myPieces
    --そのポジションに移動できる駒
    enemyCanMvPieces pos = List.filter (\piece ->
                                   List.member pos (movePoses ENEMY piece)
                                 ) enemyPieces

    --動かさなければ、取られてしまう駒
    ensures = List.filter (\enemyPiece -> List.member enemyPiece.pos myCanMvPlaces) enemyPieces
    myEnsures = List.map (\piece -> myCanMvPieces piece.pos) ensures |> List.concat
    enemyEnsures = List.map (\piece -> enemyCanMvPieces piece.pos) myEnsures |> List.concat

    _ = List.map (\piece -> let _ = Debug.log "enemyLogic:" piece in piece) enemyEnsures
  in
  pieces