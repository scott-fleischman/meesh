module Main exposing (..)

import Browser
import Html exposing (Html, Attribute, div, input, text, button)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)

main =
  Browser.sandbox { init = init, update = update, view = view }

type alias Model =
  { horizontalNames : List String
  , verticalNames : List String
  }

init : Model
init =
  { horizontalNames = [ "" ]
  , verticalNames = [ "" ]
  }

type Msg
  = AddHorizontalNameAtBottom
  | ChangeHorizontalName Int String
  | AddVerticalNameAtBottom
  | ChangeVerticalName Int String

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

update : Msg -> Model -> Model
update msg model =
  case msg of
    AddHorizontalNameAtBottom ->
      { model | horizontalNames = model.horizontalNames ++ [""] }
    ChangeHorizontalName index newName ->
      { model | horizontalNames = updateListItem index newName model.horizontalNames }
    AddVerticalNameAtBottom ->
      { model | verticalNames = model.verticalNames ++ [""] }
    ChangeVerticalName index newName ->
      { model | verticalNames = updateListItem index newName model.verticalNames }

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
    (  viewNamesSection "Horizontal Names:" ChangeHorizontalName AddHorizontalNameAtBottom model.horizontalNames
    ++ viewNamesSection "Vertical Names:"   ChangeVerticalName   AddVerticalNameAtBottom   model.verticalNames
    )
