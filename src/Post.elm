module Post exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as DP exposing (decode, required, optional, custom)
import Html exposing (Html, text, div, button, p, a, img)
import Html.Attributes exposing (..)
import Http exposing (..)
import HttpBuilder exposing (..)
import Regex exposing (contains, regex)


type alias SearchResponseBody =
    { posts : List Post
    }


type alias Post =
    { author : String
    , title : String
    , url : String
    , isVideo : Bool
    , media : Maybe Media
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
    , thumbnail : String
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
                |> DP.optional "media" (Decode.map Just decodeMedia) Nothing
             --decodeMedia Nothing
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
        |> DP.required "thumbnail_url" Decode.string


search : String -> Http.Request SearchResponseBody
search search =
    "https://www.reddit.com/r/compsci/search.json?q="
        ++ search
        |> HttpBuilder.get
        |> withExpect (Http.expectJson decodeRP)
        |> HttpBuilder.toRequest

viewPost : Post -> Html msg
viewPost post =
    div [ class "post" ]
        [ viewMedia post
        , p []
            [ text post.author ]
        , p []
            [ text post.title ]
        , p []
            [ a [ href post.url ]
                [ text post.url ]
            ]
        ]


viewMedia : Post -> Html msg
viewMedia { url, isVideo, media } =
    let
        extension =
            String.split "." url
                |> List.reverse
                |> List.head

        isImage =
            case extension of
                Just str ->
                    contains (regex "(gif$)|(png)|(jpg)") str

                Nothing ->
                    False
    in
        case isImage of
            True ->
                img [ src url, width 120 ] []

            False ->
                div [] []
