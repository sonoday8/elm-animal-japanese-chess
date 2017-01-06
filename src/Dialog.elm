module Dialog exposing(..)

import Html exposing (..)
import Html.Attributes exposing (style, class)
import Html.Events exposing (on, onClick, onWithOptions, Options)

import Types exposing(Model, Msg(..), Own(..), Type(..), Position, Piece)

type alias FooterModel =
  {
    text: String
  }

type Row =
  BGAlpha Float
  | HeaderStr String
  | Body String
  | Footer FooterModel

maybeBGAlpha config =
  List.head (List.filter (
     \conf -> case conf of
       BGAlpha alpha -> True
       _ -> False) config)

dialogStyle : List Row -> List (String, String)
dialogStyle config =
  let
    alpha =
      case maybeBGAlpha config of
        Just a -> case a of
          BGAlpha alpha -> alpha
          _ -> 0
        Nothing -> 0
  in
  [
    ("background-color", "rgba(0,0,0,"++ toString alpha ++")")
    , ("position", "fixed")
    , ("top", "0"), ("bottom", "0"), ("left", "0"), ("right", "0")
    , ("display", "flex")
    , ("flex-wrap", "wrap")
    , ("justify-content", "center")
    , ("align-items", "center")
  ]

dialogBoardStyle : List (String, String)
dialogBoardStyle =
  [
    ("background-color", "#fff")
    , ("width", "auto")
    , ("height", "auto")
    , ("padding", "2px 10px")
    , ("border-radius", "6px")
    , ("display", "flex")
    , ("flex-direction", "column")
    , ("justify-content", "space-between")
    , ("align-items", "center")
  ]

dialogHeaderStyle : List (String, String)
dialogHeaderStyle =
  [
    ("width", "100%")
    , ("display", "flex")
    , ("flex-direction", "row")
    , ("justify-content", "space-between")
  ]

dialogCloseButtonStyle : List (String, String)
dialogCloseButtonStyle =
  [
    ("color", "#000")
    , ("opacity", ".2")

    , ("margin", "0")
    , ("padding", "0")
    , ("cursor", "pointer")
    , ("border", "none")
    , ("background", "none")
    , ("-webkit-appearance", "none")
    , ("outline", "none")
  ]

dialogDiv : List Row -> Html Msg
dialogDiv config =
  div [ class "dialog", style <| dialogStyle config]
    [
      div [ class "dialog-board", style dialogBoardStyle] [
        div [ class "dialog-header", style dialogHeaderStyle] [
          div [] [text ""]
          , div [ style [("width", "10px")]] []
          , button [ style dialogCloseButtonStyle, (onClick NoPromote)
        ] [ text "x" ] ]
        , div [] [ text "a"]
        , div [] [ text "b"]
      ]
    ]

view : List Row -> Bool -> Html Msg
view config isDialog =
  if isDialog then
    dialogDiv config
  else
    div [] []