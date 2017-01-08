module Dialog exposing(..)

import Html exposing (..)
import Html.Attributes exposing (style, class)
import Html.Events exposing (on, onClick, onWithOptions, Options)

import Types exposing(Model, Msg(..), Own(..), Type(..), Position, Piece)

type Config =
  BGAlpha Float
  | DialogHeader (List (Html Msg))
  | DialogBody (List (Html Msg))
  | DialogFooter (List (Html Msg))

dialogStyle : List Config -> List (String, String)
dialogStyle config =
  let
    maybeValue_ config =
      List.filter (
         \conf -> case conf of
           BGAlpha a -> True
           _ -> False) config |> List.head
    alpha =
      case maybeValue_ config of
        Just a -> case a of
          BGAlpha alpha -> alpha
          _ -> 0.6
        Nothing -> 0.6
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

headerBody : List Config -> List (Html Msg)
headerBody config =
  let
    maybeValue_ config =
      List.filter (
         \conf -> case conf of
           DialogHeader a -> True
           _ -> False) config |> List.head
  in
  case maybeValue_ config of
    Just a -> case a of
      DialogHeader a_ -> a_
      _ -> []
    Nothing -> []

bodyBody : List Config -> List (Html Msg)
bodyBody config =
  let
    maybeValue_ config =
      List.filter (
         \conf -> case conf of
           DialogBody a -> True
           _ -> False) config |> List.head
  in
  case maybeValue_ config of
    Just a -> case a of
      DialogBody a_ -> a_
      _ -> []
    Nothing -> []

footerBody : List Config -> List (Html Msg)
footerBody config =
  let
    maybeValue_ config =
      List.filter (
         \conf -> case conf of
           DialogFooter a -> True
           _ -> False) config |> List.head
  in
  case maybeValue_ config of
    Just a -> case a of
      DialogFooter a_ -> a_
      _ -> []
    Nothing -> []

dialogDiv : List Config -> Msg -> Html Msg
dialogDiv config closeMsg =
  div [ class "dialog", style <| dialogStyle config]
    [
      div [ class "dialog-board", style dialogBoardStyle] [
        div [ class "dialog-header", style dialogHeaderStyle] [
          div [] (headerBody config)
          , div [ style [("width", "10px")]] []
          , button [ style dialogCloseButtonStyle, (onClick closeMsg )
        ] [ text "x" ] ]
        , div [] (bodyBody config)
        , div [] (footerBody config)
      ]
    ]

view : List Config -> Msg -> Bool -> Html Msg
view config closeMsg isDialog =
  if isDialog then
    dialogDiv config closeMsg
  else
    div [] []