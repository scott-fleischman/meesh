module Main exposing (..)

import Array exposing (Array)
import Browser
import Html exposing (Html, Attribute, div, input, text, button)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import String

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

type alias InputNames =
  { horizontalNames : Array String
  , verticalNames : Array String
  }

type alias OffsetPair =
  { offsetSameAxis : Int
  , offsetOtherAxis : Int
  }

type alias CrosswordName =
  { name : String
  , availableIndexes : Array Int
  }

type alias CrosswordBuilder =
  { horizontalCrosswordNames : Array CrosswordName
  , verticalCrosswordNames : Array CrosswordName
  }

emptyCrosswordBuilder =
  { horizontalCrosswordNames = Array.empty
  , verticalCrosswordNames = Array.empty
  }

type alias Solution =
  { horizontalNames : Array (String, OffsetPair)
  , verticalNames : Array (String, OffsetPair)
  }

type alias CrosswordPossibilities =
  { solutions : Array Solution
  }

type alias Model =
  { inputNames : InputNames
  , maybeCrosswordPossibilities : Maybe CrosswordPossibilities
  }

singletonEmptyString : Array String
singletonEmptyString = Array.initialize 1 (\_ -> "")

init : () -> (Model, Cmd Msg)
init _ =
  ( { inputNames =
      { horizontalNames = singletonEmptyString
      , verticalNames = singletonEmptyString
      }
      , maybeCrosswordPossibilities = Nothing
    }
  , Cmd.none
  )

type Msg
  = AddHorizontalNameAtBottom
  | ChangeHorizontalName Int String
  | AddVerticalNameAtBottom
  | ChangeVerticalName Int String
  | CalculateCrossword

calculate : InputNames -> CrosswordPossibilities
calculate _ = { solutions = Array.empty }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    AddHorizontalNameAtBottom ->
      let inputNames = model.inputNames
      in ({ model | inputNames = { inputNames | horizontalNames = Array.append inputNames.horizontalNames singletonEmptyString } }, Cmd.none)
    ChangeHorizontalName index newName ->
      let inputNames = model.inputNames
      in ({ model | inputNames = { inputNames | horizontalNames = Array.set index newName inputNames.horizontalNames } }, Cmd.none)
    AddVerticalNameAtBottom ->
      let inputNames = model.inputNames
      in ({ model | inputNames = { inputNames | verticalNames = Array.append inputNames.verticalNames singletonEmptyString } }, Cmd.none)
    ChangeVerticalName index newName ->
      let inputNames = model.inputNames
      in ({ model | inputNames = { inputNames | verticalNames = Array.set index newName inputNames.verticalNames } }, Cmd.none)
    CalculateCrossword ->
      ({ model | maybeCrosswordPossibilities = Just (calculate model.inputNames) }, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

viewNames : (Int -> String -> Msg) -> Array String -> List (Html Msg)
viewNames updateName names =
  let perItem index name = div [] [ input [ placeholder "Name", value name, onInput (updateName index) ] [] ]
  in Array.toList (Array.indexedMap perItem names)

viewNamesSection : String -> (Int -> String -> Msg) -> Msg -> Array String -> List (Html Msg)
viewNamesSection label updateName addName names =
  let
    horizontalInputs : List (Html Msg)
    horizontalInputs = viewNames updateName names
  in (  [ div [] [ text label ]
        ]
    ++ horizontalInputs
    ++ [ div [] [ button [ onClick addName ] [ text "+" ] ] ]
    )

view : Model -> Html Msg
view model =
  div []
    (  viewNamesSection "Horizontal Names:" ChangeHorizontalName AddHorizontalNameAtBottom model.inputNames.horizontalNames
    ++ viewNamesSection "Vertical Names:"   ChangeVerticalName   AddVerticalNameAtBottom   model.inputNames.verticalNames
    ++ [ div [] [ button [ onClick CalculateCrossword ] [ text "Create" ] ] ]
    )
