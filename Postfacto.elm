module Postfacto exposing(..)

import Html exposing(..)
import Html.Attributes exposing(..)
import Html.Events exposing(..)
import String exposing(..)


type alias Model =
    {
        showHide : String,
        current :  String,
        added : List(String)
    }

model : Model
model  =
    {
        showHide = "none",
        current = "",
        added = []
    }


type Msg = Change String|
            Add

update: Msg -> Model -> Model
update msg model =
        case msg of
            Change content->
                case ( content |> String.trim |> String.length) > 0 of
                    True -> { model | showHide = "inline", current = content |> String.trim }
                    False -> { model | showHide = "none" , current = content|> String.trim }
            Add -> {model | showHide = "none", current = "", added = model.added ++ [model.current]}

view : Model -> Html Msg
view model =  div[]
              [
                div [] (inputBox model.showHide),
                div [] [(addValues model.added)]
              ]

main = Html.beginnerProgram { model = model, view = view, update = update }

addValues : List(String) -> Html Msg
addValues values =
    div[] (List.map divForValue values)

divForValue : String -> Html Msg
divForValue value = div [] [span [] [text value]]

inputBox : String -> List (Html Msg)
inputBox showHide = [input [placeholder "Enter something...", onInput Change][text model.current],
                     i[showHideStyle showHide, onClick Add ][text "Add"]]

showHideStyle: String  -> Attribute msg
showHideStyle input =
    style
        [
            ("display", input)
        ]