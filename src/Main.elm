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
    = SearchPosts
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
    let
        defaultSearch = "senpai"
    in
        ( Model [] "Initialised" defaultSearch, searchPosts defaultSearch)


view : Model -> Html Msg
view model =
    div []
        [ viewHeader model.searchFilter
        , div [ class "view-body" ] (List.map viewPost model.posts)
        , Html.br [] []
        -- , text model.log
        ]


viewHeader : String -> Html Msg
viewHeader searchFilter =
    Html.form [ class "header-bar", onSubmit SearchPosts ]
        [ Html.input
            [ type_ "text"
            , onChange_ UpdateSearchFilter
            , defaultValue searchFilter
            , class "text-area"
            ]
            []
        , button
            [ onClick SearchPosts
            , class "btn-r fas fa-search"
            ]
            []
        ]


onChange_ : (String -> msg) -> Html.Attribute msg
onChange_ tagger =
    on "change" (Decode.map tagger targetValue)

searchPosts searchFilter =
    Post.search searchFilter |> Http.send GetPosts

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
        case msg of
            SearchPosts ->
                ( model, searchPosts model.searchFilter )

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
