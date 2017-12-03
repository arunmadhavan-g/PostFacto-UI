module Postfacto exposing(..)

import Html exposing(..)
import Html.Attributes exposing(..)
import Html.Events exposing(..)
import String exposing(..)

type alias Post =
    {
        id: Int,
        value : String,
        likes : Int
    }
type alias Pillar =
    {
        showHide : String,
        current :  String,
        added : List(Post)
    }

model : Pillar
model  =
    {
        showHide = "none",
        current = "",
        added = []
    }


type Msg = Change String|
            Add String|
            Delete Int|
            UpVote Int

update: Msg -> Pillar -> Pillar
update msg model =
        case msg of
            Change content->
                case ( content |> String.trim |> String.length) > 0 of
                    True -> { model | showHide = "inline", current = content}
                    False -> { model | showHide = "none", current = content}
            Add content -> {model | showHide = "none", current = "", added = model.added ++ [{id = (nextId model.added) ,value = content, likes = 0}]}
            Delete id -> {model |  added = (List.filter (\a-> a.id /= id) model.added)}
            UpVote id -> {model | added = (updateUpVote model.added id) }

view : Pillar -> Html Msg
view model =  div[]
              [
                div [] (inputBox model),
                div [] [(addValues model.added)]
              ]

main = Html.beginnerProgram { model = model, view = view, update = update }

updateUpVote: List(Post) -> Int -> List(Post)
updateUpVote posts id =
        let post = posts |> List.filter (\a -> a.id == id) |> List.head
        in case post of
            Just postVal -> List.filter (\a -> a.id /= id) posts ++ [{postVal | likes = postVal.likes+1}] |> List.sortBy .id
            Nothing ->  List.filter (\a -> a.id /= id) posts |> List.sortBy .id

addValues : List(Post) -> Html Msg
addValues values =
    div[] (List.map divForPost values)

divForPost : Post -> Html Msg
divForPost post = div [class "card", style [ ("width","20rem") ] ] [div[class "card-body"][
                                                                 p[class "card-text"][text post.value],
                                                                 deleteButton post,
                                                                 upVoteButton post
                                                               ]
                                                           ]

deleteButton: Post -> Html Msg
deleteButton post = span [onClick (Delete post.id), class "oi oi-trash"][]

upVoteButton: Post -> Html Msg
upVoteButton post = div[] [
                            span [onClick (UpVote post.id), class "oi oi-thumb-up"][],
                            h6[][span[class "badge badge-secondary"][text (Basics.toString post.likes)]]
                          ]

inputBox : Pillar -> List (Html Msg)
inputBox model = [input [placeholder "Enter something...", onInput Change, value model.current][],
                  span[showHideStyle model.showHide, onClick (Add model.current), class "oi oi-check"][]]

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


