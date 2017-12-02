module Postfacto exposing(..)

import Html exposing(..)
import Html.Attributes exposing(..)
import Html.Events exposing(..)
import String exposing(..)

type alias Post =
    {
        id: Int,
        value : String
    }
type alias Model =
    {
        showHide : String,
        current :  String,
        added : List(Post)
    }


model : Model
model  =
    {
        showHide = "none",
        current = "",
        added = []
    }


type Msg = Change String|
            Add String|
            Delete Int

update: Msg -> Model -> Model
update msg model =
        case msg of
            Change content->
                case ( content |> String.trim |> String.length) > 0 of
                    True -> { model | showHide = "inline", current = content}
                    False -> { model | showHide = "none", current = content}
            Add content -> {model | showHide = "none", current = "", added = model.added ++ [{id = (nextId model.added) ,value = content}]}
            Delete id -> {model |  added = (List.filter (\a-> a.id /= id) model.added)}
view : Model -> Html Msg
view model =  div[]
              [
                div [] (inputBox model),
                div [] [(addValues model.added)]
              ]

main = Html.beginnerProgram { model = model, view = view, update = update }

addValues : List(Post) -> Html Msg
addValues values =
    div[] (List.map divForPost values)

divForPost : Post -> Html Msg
divForPost post = div [] [span [] [text post.value], deleteButton post]

deleteButton: Post -> Html Msg
deleteButton post = i [onClick (Delete post.id)][text "delete"]


inputBox : Model -> List (Html Msg)
inputBox model = [input [placeholder "Enter something...", onInput Change, value model.current][],
                     i[showHideStyle model.showHide, onClick (Add model.current)][text "Add"]]

showHideStyle: String  -> Attribute msg
showHideStyle input =
    style
        [
            ("display", input)
        ]

nextId : List(Post) -> Int
nextId  posts  = case posts of
                    [] -> 0
                    _ ->  let
                            head = (posts |> List.reverse |> List.head )
                          in
                            case head of
                                Nothing -> 0
                                Just val -> val.id + 1


