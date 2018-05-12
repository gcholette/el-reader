module Main exposing (..)

import Html exposing (Html, text, div, button, p, a, img)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Http exposing (..)
import HttpBuilder exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as DP exposing (decode, required, optional, custom)


type alias Model =
    { posts : List Post
    , log : String
    }


type Msg
    = BtnPress
    | GetPosts (Result Http.Error SearchResponseBody)


type alias SearchResponseBody =
    { posts : List Post
    }

type alias Post =
    { author : String
    , title : String
    , url : String
    , isVideo : Bool
    , media : Maybe Media -- Maybe Media
    }

type alias Media =
    { redditVideo : Maybe RedditVideo
    , oembed : Maybe OEmbed
    }

type alias RedditVideo =
    { dashUrl : String
    }

type alias OEmbed =
    { title : String
    }


identity : a -> a
identity =
    (\x -> x)


decodeRP : Decoder SearchResponseBody
decodeRP =
    decode identity
        |> DP.required "data"
            (decode SearchResponseBody
                |> DP.required "children" (Decode.list decodePost)
            )


decodePost : Decoder Post
decodePost =
    decode identity
        |> DP.required "data"
            (decode Post
                |> DP.required "author" Decode.string
                |> DP.required "title" Decode.string
                |> DP.required "url" Decode.string
                |> DP.required "is_video" Decode.bool
                |> DP.optional "media" (Decode.map Just decodeMedia) Nothing --decodeMedia Nothing
            )

decodeMedia : Decoder Media
decodeMedia =
    decode Media
        |> DP.optional "reddit_video" (Decode.map Just decodeRedditVideo) Nothing
        |> DP.optional "oembed" (Decode.map Just decodeOEmbed) Nothing


decodeRedditVideo : Decoder RedditVideo
decodeRedditVideo =
    decode RedditVideo
        |> DP.required "dash_url" Decode.string


decodeOEmbed : Decoder OEmbed
decodeOEmbed =
    decode OEmbed
        |> DP.required "title" Decode.string


get : Http.Request SearchResponseBody
get =
    "https://www.reddit.com/r/compsci/search.json?q=senpai"
        |> HttpBuilder.get
        |> withExpect (Http.expectJson decodeRP)
        |> HttpBuilder.toRequest


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
    ( Model [] "Initialised" , Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ button
            [ onClick BtnPress
            , class "btn-r"
            ]
            [ text "click me bitch i dare you"
            ]
        , Html.br [] []
        , div [] (List.map viewPost model.posts)
        , Html.br [] []
        , text model.log
        ]


viewPost : Post -> Html Msg
viewPost { author, title, url } =
    div [ class "post" ]
        [ img [ src url, width 120 ] []
        , p []
            [ text author ]
        , p []
            [ text title ]
        , p []
            [ a [ href url ] 
                [ text url ] 
            ]
        ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        httpget =
            get |> Http.send GetPosts
    in
        case msg of
            BtnPress ->
                ( model, httpget )

            GetPosts (Ok content) ->
                ( { model | posts = content.posts
                          , log = toString content 
                  }, Cmd.none )

            GetPosts (Err err) ->
                ( { model | log = toString err }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
