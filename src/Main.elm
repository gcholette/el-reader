module Main exposing (..)

import Html exposing (Html, text, div, button, p, a, img)
import Navigation exposing (Location, programWithFlags)
import Json.Decode as Decode exposing (Decoder)
import Route exposing (Route)
import Http exposing (Error)
import Task
import Pages.Feed as FeedPage exposing (..)


type alias Model =
    { activePage : Page
    , log : String
    }


type Page
    = Root
    | Feed FeedPage.Model


type Msg
    = SetRoute (Maybe Route)
    | FeedLoaded (Result Http.Error FeedPage.Model)
    | FeedMsg FeedPage.Msg


main : Program Decode.Value Model Msg
main =
    programWithFlags (Route.fromLocation >> SetRoute)
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


view : Model -> Html Msg
view model =
    case model.activePage of
        Root ->
            a [ (Route.href Route.Feed) ] [ text "come it's fun here" ]

        Feed submodel ->
            FeedPage.view submodel
                |> Html.map FeedMsg


init : Decode.Value -> Location -> ( Model, Cmd Msg )
init val location =
    let
        initialModel =
            Model (Feed FeedPage.initialModel) ""

        --(Feed FeedPage.initialModel)
    in
        setRoute (Route.fromLocation location) initialModel


setRoute : Maybe Route -> Model -> ( Model, Cmd Msg )
setRoute route model =
    case route of
        Nothing ->
            ( model, Cmd.none )

        Just Route.Root ->
            ( { model | activePage = Root }, Cmd.none )

        Just Route.Feed ->
            ( { model | activePage = Feed FeedPage.initialModel }, Task.attempt FeedLoaded FeedPage.init )



--Task.attempt FeedMsg FeedPage.init)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    updatePage model.activePage msg model


updatePage : Page -> Msg -> Model -> ( Model, Cmd Msg )
updatePage page msg model =
    let
        toPage toModel toMsg subUpdate subMsg subModel =
            let
                ( newModel, newCmd ) =
                    subUpdate subMsg subModel
            in
                ( { model | activePage = (toModel newModel) }, Cmd.map toMsg newCmd )
    in
        case ( msg, page ) of
            ( SetRoute route, _ ) ->
                setRoute route model

            ( FeedMsg subMsg, Feed subModel ) ->
                toPage Feed FeedMsg (FeedPage.update) subMsg subModel

            ( FeedLoaded (Ok subModel), _ ) ->
                ( { model | activePage = Feed subModel }, Cmd.none )
                
            ( FeedLoaded (Err error), _ ) ->
                ( { model | log = toString error}, Cmd.none )

            ( _, _ ) ->
                ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
