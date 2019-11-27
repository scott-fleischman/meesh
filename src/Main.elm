module Main exposing (..)

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
  { horizontalNames : List String
  , verticalNames : List String
  }

type alias CrosswordPossibilities =
  {
  }

type alias Model =
  { inputNames : InputNames
  , maybeCrosswordPossibilities : Maybe CrosswordPossibilities
  }

init : () -> (Model, Cmd Msg)
init _ =
  ( { inputNames =
      { horizontalNames = [ "" ]
      , verticalNames = [ "" ]
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

updateListItem : Int -> a -> List a -> List a
updateListItem index item list =
  if index < 0
    then list
    else
      case list of
        [] -> []
        x :: xs -> 
          if index == 0
            then item :: xs
            else x :: updateListItem (index - 1) item xs

calculate : InputNames -> CrosswordPossibilities
calculate _ = {}

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    AddHorizontalNameAtBottom ->
      let inputNames = model.inputNames
      in ({ model | inputNames = { inputNames | horizontalNames = inputNames.horizontalNames ++ [""] } }, Cmd.none)
    ChangeHorizontalName index newName ->
      let inputNames = model.inputNames
      in ({ model | inputNames = { inputNames | horizontalNames = updateListItem index newName inputNames.horizontalNames } }, Cmd.none)
    AddVerticalNameAtBottom ->
      let inputNames = model.inputNames
      in ({ model | inputNames = { inputNames | verticalNames = inputNames.verticalNames ++ [""] } }, Cmd.none)
    ChangeVerticalName index newName ->
      let inputNames = model.inputNames
      in ({ model | inputNames = { inputNames | verticalNames = updateListItem index newName inputNames.verticalNames } }, Cmd.none)
    CalculateCrossword ->
      ({ model | maybeCrosswordPossibilities = Just (calculate model.inputNames) }, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

viewNames : (Int -> String -> Msg) -> Int -> List String -> List (Html Msg)
viewNames updateName index names =
  case names of
    [] -> []
    name :: remainingNames ->
      div [] [ input [ placeholder "Name", value name, onInput (updateName index) ] [] ]
      :: viewNames updateName (index + 1) remainingNames

viewNamesSection : String -> (Int -> String -> Msg) -> Msg -> List String -> List (Html Msg)
viewNamesSection label updateName addName names =
  let
    horizontalInputs : List (Html Msg)
    horizontalInputs = viewNames updateName 0 names
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
