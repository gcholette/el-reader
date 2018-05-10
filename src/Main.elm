import Html exposing (Html, text, div, button)
import Html.Events exposing (..)
import Http exposing (..)
import HttpBuilder exposing (..)

type alias Model = String

type Msg = BtnPress | HttpGET (Result Http.Error String)


get : Http.Request String 
get =
    "http://localhost:3005/books"
        |> HttpBuilder.get
        |> withExpect Http.expectString
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

            HttpGET (Ok str) -> 
                (str, Cmd.none)

            HttpGET (Err err) -> 
                ((toString err), Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none