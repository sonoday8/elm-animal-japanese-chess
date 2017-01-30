import Html exposing (Html, beginnerProgram, div, text, button)
import Html.Attributes exposing (style, draggable)
import Html.Events exposing (on, onClick, onWithOptions, Options)
import Json.Decode as Json

import Dialog exposing(..)
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
    , promotePos = Nothing
    , win = Nothing
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
        maybePiece = Func.maybeGetPiece pos pieces
        win = Nothing
        promotePos =
          case win of
            Just own -> Nothing
            Nothing ->
              case maybePiece of
                Just piece ->
                  if Func.isEnemyFieldPos pos piece.own && piece.p_type == CHICK then
                    Just pos
                  else
                    Nothing
                Nothing -> Nothing
      in
        { model
        | turn= if Func.isJust promotePos then model.turn else Func.changeTurn model.turn
        , drag=Nothing
        , isDropFields=[]
        , pieces=pieces
        , promotePos=promotePos
        }

    Promoted pos ->
      let
        pieces = Func.promotePieces pos model.pieces
      in
      {model
        | turn= Func.changeTurn model.turn
        , pieces = pieces
        , promotePos=Nothing}

    _ -> model

view : Model -> Html Msg
view model =
  let
    promoteDialogConfig = [
      DialogBody [text "成る？"]
      , DialogFooter [
          button [style [], (onClick (Promoted Nothing) )] [ text "NO" ]
          , button [style [], (onClick (Promoted model.promotePos))] [ text "YES" ]
        ]
      ]
    promoteDialog flg = Dialog.view promoteDialogConfig (Promoted Nothing) flg

    --winDialogConfig = [DialogBody [text "勝利！"]]
    --winDialog flg =

    turn_ = if model.turn == MY then "my" else "enemy"
    attributes =
      [
        style [
          ("display", "flex"), ("flex-wrap", "wrap")
          , ("width", "450px"), ("height", "450px")
        ]
      ]
    _ = if model.turn == ENEMY then Func.enemyLogic model.pieces else model.pieces
  in
  div [] [
    div [] [ text turn_]
    , div attributes (View.viewFields model)
    , promoteDialog (Func.isJust model.promotePos)
  ]


