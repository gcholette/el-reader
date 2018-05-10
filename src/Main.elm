import Html exposing (Html, text, div, button)
import Html.Events exposing (..)
import Http exposing (..)
import HttpBuilder exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, required, optional)


type alias Model = String

type Msg 
    = BtnPress 
    | HttpGET (Result Http.Error PageContent)


type alias PageContent =
    { kind : String
    }


get : Http.Request PageContent 
get =
    "https://www.reddit.com/r/compsci/search.json?q=show"
        |> HttpBuilder.get
        |> withExpect (Http.expectJson decoder)
        |> HttpBuilder.toRequest



decoder : Decoder PageContent
decoder =
    decode PageContent
        |> required "kind" Decode.string


listDecoder : Decoder (List PageContent)
listDecoder =
    Decode.list decoder


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
    

init : ( Model, Cmd Msg )
init = ("Dude. plsd.", Cmd.none)

view : Model -> Html Msg
view model = 
    div [] [ button [ onClick BtnPress ] [ text "click me bitch i dare you"], text model ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        httpget = get |> Http.send HttpGET
        
    in
        case msg of 
            BtnPress ->
                (model, httpget)

            HttpGET (Ok content) -> 
                ((toString content), Cmd.none)

            HttpGET (Err err) -> 
                ((toString err), Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none