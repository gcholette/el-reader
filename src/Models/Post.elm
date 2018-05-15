module Models.Post exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as DP exposing (decode, required, optional, custom)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import HttpBuilder exposing (..)
import Regex exposing (contains, regex)


type alias SearchResponseBody =
    { posts : List Post
    }


type alias Post =
    { author : String
    , title : String
    , selfText : String
    , subreddit : String
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
                |> DP.required "selftext" Decode.string
                |> DP.required "subreddit_name_prefixed" Decode.string
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


fileExtension : String -> Maybe String
fileExtension url =
    String.split "." url
        |> List.reverse
        |> List.head


viewPost : Post -> Html msg
viewPost post =
    div [ class "post" ]
        [ viewMedia post
        , viewBody post
        ]


onBlur_ : (String -> msg) -> Html.Attribute msg
onBlur_ tagger =
    on "blur" (Decode.map tagger targetValue)


viewMedia : Post -> Html msg
viewMedia { url, isVideo, media } =
    let
        isImage =
            case (fileExtension url) of
                Just str ->
                    contains (regex "(^gif$)|(^png$)|(^jpg$)") str

                Nothing ->
                    False
    in
        case isImage of
            True ->
                div [ class "post-preview" ]
                    [ img [ src url, class "thumbnail" ] [] ]

            False ->
                div [] []


viewBody : Post -> Html msg
viewBody { author, title, selfText, url, subreddit } =
    div [ class "post-body" ]
        [ div [ class "post-title" ] [ text title ]
        , div [ class "post-author" ] [ text author ]
        , div [ class "post-subreddit" ] [ text subreddit ]
        , if (selfText /= "") then
            (div [ class "post-selfText" ] [ text selfText ])
          else
            div [] []
        ]
