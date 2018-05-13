module Main exposing (..)

import Html exposing (Html, text, div, button, p, a, img)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Http exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Post exposing (..)


type alias Model =
    { posts : List Post
    , log : String
    , searchFilter : String
    }


type Msg
    = BtnPress
    | GetPosts (Result Http.Error SearchResponseBody)
    | UpdateSearchFilter String


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Msg )
init =
    ( Model [] "Initialised" "senpai", Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ button
            [ onClick BtnPress
            , class "btn-r"
            ]
            [ text "click me bitch i dare you"
            ]
        , Html.input
            [ type_ "text"
            , onBlur_ UpdateSearchFilter
            , defaultValue model.searchFilter
            ]
            []
        , Html.br [] []
        , div [] (List.map viewPost model.posts)
        , Html.br [] []
        , text model.log
        ]


onBlur_ : (String -> msg) -> Html.Attribute msg
onBlur_ tagger =
    on "blur" (Decode.map tagger targetValue)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        httpget =
            Post.search model.searchFilter |> Http.send GetPosts
    in
        case msg of
            BtnPress ->
                ( model, httpget )

            GetPosts (Ok content) ->
                ( { model
                    | posts = content.posts
                    , log = toString content
                  }
                , Cmd.none
                )

            GetPosts (Err err) ->
                ( { model | log = toString err }, Cmd.none )

            UpdateSearchFilter searchFilter ->
                ( { model | searchFilter = searchFilter }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
