module Route exposing (Route(..), href, modifyUrl, fromLocation)

import Html exposing (Attribute)
import Html.Attributes as Attr
import Navigation exposing (Location)
import UrlParser as Url exposing ((</>), Parser, oneOf, parseHash, s, string)


type Route
    = Root
    | Feed


route : Parser (Route -> a) a
route =
    oneOf
        [ Url.map Root (s "")
        , Url.map Feed (s "feed")
        ]


routeToString : Route -> String
routeToString page =
    let
        pieces =
            case page of
                Root ->
                    [ "" ]

                Feed ->
                    [ "feed" ]
    in
        "#/" ++ String.join "/" pieces



-- PUBLIC HELPERS --


href : Route -> Attribute msg
href route =
    Attr.href (routeToString route)


modifyUrl : Route -> Cmd msg
modifyUrl =
    routeToString >> Navigation.modifyUrl


fromLocation : Location -> Maybe Route
fromLocation location =
    if String.isEmpty location.hash then
        Just Root
    else
        parseHash route location
