import Html exposing (Html, beginnerProgram, div, text)
import Html.Attributes exposing (style, draggable)
import Html.Events exposing (on, onWithOptions, Options)
import Json.Decode as Json

import Types exposing(Model, Msg(..), Own(..), Type(..), Position, Piece)
import Func
import View exposing(..)


main =
  Html.beginnerProgram
    { model = model
    , view = view
    , update = update
    }

model : Model
model =
    {
    turn=MY
    , drag=Nothing
    , isDropFields=[]
    , pieces= [
         {own=ENEMY,p_type=ELEP,pos={x=2,y=2}}
        ,{own=ENEMY,p_type=LION,pos={x=1,y=2}}
        ,{own=ENEMY,p_type=GIRA,pos={x=0,y=2}}
        ,{own=ENEMY,p_type=CHICK,pos={x=1,y=3}}
        ,{own=MY,p_type=CHICK,pos={x=1,y=4}}
        ,{own=MY,p_type=GIRA,pos={x=2,y=5}}
        ,{own=MY,p_type=LION,pos={x=1,y=5}}
        ,{own=MY,p_type=ELEP,pos={x=0,y=5}}
    ]
    }

update : Msg -> Model -> Model
update msg model =
  case msg of
    DragStart pos ->
        let
          dropFields : List Position
          dropFields =
            case (Func.maybeGetPiece pos model.pieces) of
              Just dragPiece -> Func.getDropFields dragPiece model
              _ -> []
        in
          { model
          | drag = Just pos
            ,isDropFields = dropFields
          }
    DragEnd ->
--        let _ = Debug.log "end:" "" in
      { model
      | isDropFields = []
      }

    DragEnter pos ->
--        let _ = Debug.log "enter:" pos in
--      { model
--      | tileOver = Just pos
--      }
        model

    Drop pos ->
      let
        pieces = Func.updatedPieces pos model
      in
        { model
        | turn= Func.changeTurn model.turn
        , drag=Nothing
        , isDropFields=[]
        , pieces=pieces
        }

    _ -> model

view : Model -> Html Msg
view model =
  let
    trun_ = if model.turn == MY then "my" else "enemy"
    attributes =
      [
        style [
          ("display", "flex"), ("flex-wrap", "wrap")
          , ("width", "450px"), ("height", "450px")
        ]
      ]
  in
  div [] [
    div [] [ text trun_]
    , div attributes (viewFiels model)
  ]


