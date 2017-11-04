module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


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


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        None ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "app-wrapper" ]
        [ section [ class "game" ] (touchpads model)
        ]


touchpads : Model -> List (Html Msg)
touchpads model =
    [ div [ class "touchpad", id "1" ] [ text "1" ]
    , div [ class "touchpad", id "2" ] [ text "2" ]
    , div [ class "touchpad", id "3" ] [ text "3" ]
    , div [ class "touchpad", id "4" ] [ text "4" ]
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
