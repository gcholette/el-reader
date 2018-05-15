module Pages.Feed exposing (..)

import Html exposing (Html, text, div, button, p, a, img)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Http exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Task exposing (..)
import Models.Post as Post exposing (..)


type alias Model =
    { log : String
    , searchFilter : String
    , posts : List Post
    }


type Msg
    = SearchPosts
    | GetPosts (Result Http.Error SearchResponseBody)
    | UpdateSearchFilter String


initialModel : Model
initialModel =
    let
        defaultSearch =
            "senpai"
    in
        Model "Initialised" defaultSearch []



{-
   init : Task Http.Error Model
   init =
       let
           searchTask searchFilter =
               Post.search searchFilter |> Http.toTask
       in
           Task.map (Model "" "") (searchTask "hey")
-}


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
    div [ class "header-bar" ]
        [ Html.i [ class "header-settings fas fa-cog" ] []
        , Html.span [ class "header-subreddit" ] [ text "/r/compsci" ]
        , Html.form [ class "search-container", onSubmit SearchPosts ]
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
