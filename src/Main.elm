import Array exposing (..)
import List exposing (..)
import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (style, draggable)
import Html.Events exposing (on, onWithOptions, Options)
import Json.Decode as Json

import Types exposing(..)
import Func exposing(..)

main =
  Html.beginnerProgram
    { model = model
    , view = view
    , update = update
    }

type alias Model = {
  turn:Bool,
  drag: Maybe Position,
  isDropFields: List Position,
  pieces:List Piece,
  myPieces: List Piece,
  enemyPieces: List Piece
  }

model : Model
model =
    {
    turn=True,
    drag=Nothing,
    isDropFields=[],
    pieces= [
         {own=ENEMY,p_type=ELEP,isDrag=False,pos={x=2,y=2}}
        ,{own=ENEMY,p_type=LION,isDrag=False,pos={x=1,y=2}}
        ,{own=ENEMY,p_type=GIRA,isDrag=False,pos={x=0,y=2}}
        ,{own=ENEMY,p_type=CHICK,isDrag=False,pos={x=1,y=3}}
        ,{own=MY,p_type=CHICK,isDrag=False,pos={x=1,y=4}}
        ,{own=MY,p_type=GIRA,isDrag=False,pos={x=2,y=5}}
        ,{own=MY,p_type=LION,isDrag=False,pos={x=1,y=5}}
        ,{own=MY,p_type=ELEP,isDrag=False,pos={x=0,y=5}}
    ],
    myPieces=[],
    enemyPieces=[]
    }

type Msg
  = DragStart Position
  | DragEnd
  | DragEnter Position
  | Drop Position
  | NoOp

update : Msg -> Model -> Model
update msg model =
  case msg of
    DragStart pos ->
        let
          dragPieces = List.filter (\piece -> piece.pos == pos) model.pieces
          dropFields =
            case (List.head dragPieces) of
              Just dragPiece -> getDropFields dragPiece
              _ -> []
        in
          { model
          | drag = Just pos
            ,isDropFields = dropFields
          }
    DragEnd ->
--        let _ = Debug.log "end:" "" in
--      { model
--      | isKnightDragged = False
--      }
        model

    DragEnter pos ->
--        let _ = Debug.log "enter:" pos in
--      { model
--      | tileOver = Just pos
--      }
        model

    Drop pos ->
      let
        dragPos = model.drag
        dropPos = pos
        pieces = updatedPieces model.pieces dragPos dropPos
      in
        { model
        |
        drag=Nothing,
        isDropFields=[],
        pieces=pieces
        }

    _ -> model

rowLength = 8
colLength = 3

fields : List Position
fields =
  Array.initialize (rowLength*colLength) (\i ->
    { x = i % colLength
    , y = i // colLength
    }
  ) |> Array.toList

viewFiels model =
  List.map (\pos ->
      let
        isEmpty = List.isEmpty (List.filter (\piece -> piece.pos == pos) model.pieces)
        name =
          case List.head (List.filter (\piece -> piece.pos == pos) model.pieces) of
          Just animal -> animal.p_type
          Nothing -> NoAnimal
        pieceDiv = div [style [("width", "100%"),("height", "100%")]
           , onDragStart (DragStart pos)
           , onDragEnd DragEnd
           , onDragEnter (DragEnter pos)
           , draggable "true"] []

        node =
            if isEmpty then
              [  ]
            else
              [ (toString pos) ++ (toString name) |> text, pieceDiv]

        isDorp = List.member pos model.isDropFields

        divAttributes =
            if isDorp then
                [ style
                  [("flex-grow", "1"),("width", "30%"), ("height", "25%")]
                , onDrop (Drop pos),dragOverPrevent NoOp]
            else
                [ style [("flex-grow", "1"),("width", "30%"), ("height", "25%")]]
      in
      div divAttributes node
  ) fields

view : Model -> Html Msg
view model =
  div [] [
  div [ style
    [
     ("display", "flex")
     , ("flex-wrap", "wrap")
     , ("width", "450px")
     , ("height", "450px")
    ]
  ] (viewFiels model)
 ]


