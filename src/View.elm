module View exposing(..)

import Html exposing (..)
--import Html.App as Html
import Html.Attributes exposing (style, draggable)
import Html.Events exposing (on, onWithOptions, Options)

import Types exposing(..)
import Func exposing(..)

-- フィールドを描画
viewFiels : Model -> List (Html Msg)
viewFiels model =
  List.map (\pos ->
      let
        node = getNode pos model.pieces
        attributes = getAttributes pos model
      in
      div attributes node
  ) fields

getAttributes : Position -> Model -> List (Attribute Msg)
getAttributes pos model =
  let
    isDrop =
      if (isReservePos pos) then
        False
      else
        List.member pos model.isDropFields
    style_ : Attribute Msg
    style_ = style [("flex-grow", "1"),("width", "30%"), ("height", "25%")]
    dropStyle_ = style [("flex-grow", "1"),("width", "30%"), ("height", "25%"), ("background-color", "pink")]
  in
    if isDrop then
    [ dropStyle_
      , onDrop (Drop pos)
      , dragOverPrevent NoOp
    ]
  else
    [ style_ ]

getNode : Position -> List Piece -> List (Html Msg)
getNode pos pieces =
  let
    maybePiece : Maybe Piece
    maybePiece = maybeGetPiece pos pieces

    name : String
    name = case maybePiece of
      Just animal -> toString animal.p_type
      Nothing -> toString NoAnimal

  in
  case maybePiece of
    Just piece ->
      [ pieceDiv piece]
    _ -> []

pieceDiv : Piece -> Html Msg
pieceDiv piece =
  let
    color = if piece.own == MY then "rgba(240,255,255,0.5)" else "rgba(250,235,215,0.5)"
    strPos = toString piece.pos

  in
  div [
    style [("width", "100%"),("height", "100%"), ("background-color", color), ("border", "#ff0000 solid 1px")]
    , onDragStart (DragStart piece.pos)
    , onDragEnd DragEnd
    , onDragEnter (DragEnter piece.pos)
    , draggable "true"] [ strPos ++ toString piece.p_type |> text]