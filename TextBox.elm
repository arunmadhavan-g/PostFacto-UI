module TextBox exposing(..)

import Html exposing(..)
import Html.Attributes exposing(..)
import Html.Events exposing(..)
import String exposing(..)


showTick : Attribute msg
showTick =
    style
    [
        ("display", "inline")
    ]

hideTick : Attribute msg
hideTick =
    style
    [
        ("display", "none")
    ]



model : Attribute msg
model  =  hideTick

type Msg = Change String

update: Msg -> Attribute msg -> Attribute msg
update (Change content) model =
        case (content |> String.trim |> String.length) > 0 of
        True -> showTick
        False -> hideTick

view : Attribute Msg -> Html Msg
view model =  div [] [
                         input [placeholder "Enter something...", onInput Change][],
                         i[model][text "Add"]
                     ]

main = Html.beginnerProgram { model = model, view = view, update = update }
