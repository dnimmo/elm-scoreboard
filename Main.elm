module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App as App


-- model


type alias Model =
  { players : List Player
  , name : String
  , playerId : Maybe Int
  , plays : List Play
  }


type alias Player =
  { id : Int
  , name : String
  , points : Int
  }


type alias Play =
  { id : Int
  , playerId : Int
  , name : String
  , points : Int
  }


initModel : Model
initModel =
  { players = []
  , name = ""
  , playerId = Nothing
  , plays = []
  }

initPlayer : Player
initPlayer =
  { id = 0
  , name = ""
  , points = 0
  }


type Msg
  = Edit Player
  | Score Player Int
  | Input String
  | Save
  | Cancel
  | DeletePlay Play


update : Msg -> Model -> Model
update msg model =
  case msg of
    Input name ->
      { model | name = name }

    Save ->
      { model | players = (addPlayer model.players model.name)
              , name = initModel.name }

    Cancel ->
      { model | name = initModel.name }

    _ ->
      model


addPlayer : List Player -> String -> List Player
addPlayer players newPlayer =
  List.reverse ({ initPlayer | name = newPlayer } :: players)


-- view


view : Model -> Html Msg
view model =
  div [ class "scoreboard" ]
      [ h1 [] [ text "Score Keeper" ]
      , playerForm model
      , p [] [ text (toString model) ]
      ]


playerForm : Model -> Html Msg
playerForm model =
  Html.form [ onSubmit Save ]
      [ input
          [ type' "text"
          , placeholder "Add/Edit Player..."
          , onInput Input
          , value model.name
          ]
          []
      , button [ type' "submit" ] [ text "Save" ]
      , button [ type' "button", onClick Cancel ] [ text "Cancel" ]
      ]


main : Program Never
main =
  App.beginnerProgram
    { model = initModel
    , view = view
    , update = update
    }
