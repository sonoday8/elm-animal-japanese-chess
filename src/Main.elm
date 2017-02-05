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

initModel : Model
initModel =
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

model : Model
model = initModel

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
        model

    Drop pos ->
      let
        pieces = Func.updatedPieces pos model
        win = Func.isWin pieces
        promotePos =
          case win of
            Just own -> Nothing
            Nothing -> Func.getPromotePos pos pieces
       in
        { model
        | turn= if Func.isJust promotePos then model.turn else Func.changeTurn model.turn
        , drag=Nothing
        , isDropFields=[]
        , pieces=pieces
        , promotePos=promotePos
        , win=win
        }

    Promoted pos ->
      let
        pieces = Func.promotePieces pos model.pieces
      in
      {model
        | turn= Func.changeTurn model.turn
        , pieces = pieces
        , promotePos=Nothing}

    Reset -> initModel

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

    winStr = case model.win of
      Just MY -> "勝ち！"
      Just ENEMY -> "負け。。"
      Nothing -> ""
    winDialogConfig = [DialogBody [text winStr]]
    winDialog flg = Dialog.view winDialogConfig Reset flg

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
    , winDialog (Func.isJust model.win)
  ]


