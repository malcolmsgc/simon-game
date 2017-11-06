module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


-- import Html.Events exposing (..)
-- MODEL


type alias Model =
    { test : String
    , strictMode : Bool
    , sequence : List Sound
    }


initModel : Model
initModel =
    Model "This is working" False []


type alias Sound =
    { id : String
    , soundUrl : String
    }


type alias Quad =
    { id : Int
    , sound : Sound
    }


init : ( Model, Cmd msg )
init =
    ( initModel, Cmd.none )



-- UPDATE


type Msg
    = None
    | StartGame
    | ToggleStrict


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        None ->
            ( model, Cmd.none )

        StartGame ->
            ( model, Cmd.none )

        ToggleStrict ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "app-wrapper" ]
        [ h1 [ class "header" ] [ text "Simon Game" ]
        , section [ class "game" ] (touchpads model)
        , footer []
            [ a [ href "https://github.com/malcolmsgc/simon-game" ] [ text "OSS project " ]
            , text "by "
            , a [ href "https://www.linkedin.com/in/malcolmcumming/" ] [ text "Malcolm Cumming" ]
            ]
        ]


touchpads : Model -> List (Html Msg)
touchpads model =
    [ div [ class "touchpad top-row", id "top-left" ] []
    , div [ class "touchpad top-row", id "top-right" ] []
    , controls model
    , div [ class "touchpad bottom-row", id "bottom-left" ] []
    , div [ class "touchpad bottom-row", id "bottom-right" ] []
    ]


controls : Model -> Html Msg
controls model =
    let
        steps =
            Nothing

        stepCount =
            case steps of
                Just int ->
                    (toString int)

                Nothing ->
                    "--"

        stepUnit =
            case steps of
                Just int ->
                    if int <= 1 then
                        "step"
                    else
                        "steps"

                Nothing ->
                    "press start"
    in
        div [ class "controls" ]
            [ div [ class "step-count" ] [ text stepCount, span [] [ text stepUnit ] ]
            , label []
                [ text "start"
                , button [ type_ "button", name "start", onClick StartGame ] []
                ]
            , label []
                [ text "strict"
                , button [ type_ "button", name "strict", onClick ToggleStrict ] []
                ]
            ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub msg
subscriptions model =
    Sub.none



-- PROGRAM


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
