module View exposing(..)

import Html exposing (..)
import Html.Attributes exposing (style, draggable, href, rel, class, src)
import Html.Events exposing (on, onClick, onWithOptions, Options)

--import Dialog

import Types exposing(Model, Msg(..), Own(..), Type(..), Position, Piece)
import Func exposing(..)

-- フィールドを描画
viewFields : Model -> List (Html Msg)
viewFields model =
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
      Nothing -> ""

  in
  case maybePiece of
    Just piece ->
      [ pieceDiv piece]
    _ -> []

pieceDivStyle = [
      ("display","-ms-flexbox")
      ,("display","-webkit-box")
      ,("display","flex")
      ,("-webkit-flex-direction","column")
      ,("flex-direction","column")
      ,("-webkit-justify-content","center")
      ,("justify-content","center")
      ,("-webkit-align-items","center")
      ,("align-items","center")
  ]

pieceDiv : Piece -> Html Msg
pieceDiv piece =
  let
    color = if piece.own == MY then "rgba(240,255,255,0.5)" else "rgba(250,235,215,0.5)"
    strPos = toString piece.pos

  in
  div [
    style (pieceDivStyle ++ [("width", "100%"),("height", "100%"), ("background-color", color), ("border", "#ff0000 solid 1px")])
    , onDragStart (DragStart piece.pos)
    , onDragEnd DragEnd
    , onDragEnter (DragEnter piece.pos)
    , draggable "true"
    ]
    (pieceImg piece)
    --[ strPos ++ toString piece.p_type |> text]
    --[ img [ src "lion.png", style [ ("height", "115px") ] ] [] ]

pieceImgStyle = [
    ("width","auto")
    ,("height","auto")
    ,("max-width","100%")
    ,("max-height","100%")
  ]

pieceImg : Piece -> List (Html Msg)
pieceImg piece =
  case piece.p_type of
    LION -> [ img [ src "lion.png", style pieceImgStyle ] [] ]
    GIRA -> [ img [ src "gira.png", style pieceImgStyle ] [] ]
    ELEP -> [ img [ src "elep.png", style pieceImgStyle ] [] ]
    CHICKEN -> [ img [ src "chicken.png", style pieceImgStyle ] [] ]
    CHICK -> [ img [ src "chick.png", style pieceImgStyle ] [] ]

