module Main exposing (..)

import Browser
import Html exposing (Html, Attribute, div, input, text, button)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)

main =
  Browser.sandbox { init = init, update = update, view = view }

type alias Model =
  { horizontalNames : List String
  }

init : Model
init =
  { horizontalNames = [ "" ] }

type Msg
  = AddHorizontalNameAtBottom
  | ChangeHorizontalName Int String

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

viewHorizontalNames : Int -> List String -> List (Html Msg)
viewHorizontalNames index names =
  case names of
    [] -> []
    name :: remainingNames ->
      div [] [ input [ placeholder "Name", value name, onInput (ChangeHorizontalName index) ] [] ]
      :: viewHorizontalNames (index + 1) remainingNames

view : Model -> Html Msg
view model =
  let
    horizontalInputs : List (Html Msg)
    horizontalInputs = viewHorizontalNames 0 model.horizontalNames
  in div []
    (  [ div [] [ text "Horizontal Names" ]
       ]
    ++ horizontalInputs
    ++ [ div [] [ button [ onClick AddHorizontalNameAtBottom ] [ text "+" ] ] ]
    )
