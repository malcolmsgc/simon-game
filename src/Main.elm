port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, on)
import Random
import Time exposing (Time, millisecond)
import Task
import Debug exposing (log)

-- import Process
-- MODEL


type alias Model =
    { test : String
    , gameActive : Bool
    , allowInput : Bool
    , strictMode : Bool
    , gameSounds : List Sound
    , sequence : List Int
    , userSequence : List Int
    , nextSoundAt : Maybe Time
    }


initModel : Model
initModel =
    { test = "This is working"
    , gameActive = False
    , allowInput = False
    , strictMode = False
    , gameSounds = gameSounds
    , sequence = []
    , userSequence = []
    , nextSoundAt = Nothing
    }


type alias Sound =
    { id : Int
    , url : String
    }


gameSounds : List Sound
gameSounds =
    [ { id = 1, url = "https://s3.amazonaws.com/freecodecamp/simonSound1.mp3" }
    , { id = 2, url = "https://s3.amazonaws.com/freecodecamp/simonSound2.mp3" }
    , { id = 3, url = "https://s3.amazonaws.com/freecodecamp/simonSound3.mp3" }
    , { id = 4, url = "https://s3.amazonaws.com/freecodecamp/simonSound4.mp3" }
    ]


init : ( Model, Cmd Msg )
init =
    ( initModel, generateSequence )



-- UPDATE


type Msg
    = None
    | NewGame
    | GenerateSequence
    | PopulateSequence (List Int)
    | ToggleStrict Bool
      -- | Tick Time
    | SelectSound Time
    | StaggerSound
    | PlaySound Float
    | AddToSequence Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        None ->
            ( model, Cmd.none )

        NewGame ->
            let
                count =
                    List.length model.userSequence

                firstRun =
                    not model.gameActive

                newModel =
                    if count > 0 then
                        { model | sequence = [], userSequence = [], allowInput = False }
                    else
                        { model | gameActive = True }
            in
                -- Needs to generate new sequence then start playback
                ( newModel, startGame firstRun )

        GenerateSequence ->
            ( model, generateSequence )

        PopulateSequence sequenceList ->
            ( { model | sequence = sequenceList }, Cmd.none )

        ToggleStrict isStrict ->
            ( { model | strictMode = (not isStrict) }, Cmd.none )

        -- Tick timeNow ->
        --     update (SelectSound timeNow) model
        SelectSound time ->
            ( model, Random.generate AddToSequence (Random.int 1 4) )

        AddToSequence id ->
            ( { model | sequence = id :: model.sequence }, Cmd.none )

        StaggerSound ->
            ( model, Random.generate PlaySound (Random.float 0 1) )

        PlaySound multiplier ->
            playSound model multiplier



-- TASKS


generateSequence : Cmd Msg
generateSequence =
    Random.list 20 (Random.int 1 4)
        |> Random.generate PopulateSequence


startSequence : Cmd Msg
startSequence =
    Cmd.none

startGame : Bool -> Cmd Msg
startGame firstRun =
    let
        cmds =
            if firstRun then
                startSequence
            else
                generateSequence
                    -- |> Task.andThen (\ -> startSequence)
    in
        cmds


time : Model -> Time -> Model
time model timeNow =
    case model.nextSoundAt of
        Nothing ->
            model

        Just playAt ->
            if timeNow <= playAt then
                model
            else
                model


playSound model multiplier =
    -- if model.
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
        , sounds model.gameSounds
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

        startBtnText =
            case model.gameActive of
                True ->
                    "reset"

                False ->
                    "start"
    in
        div [ class "controls" ]
            [ div [ class "step-count" ] [ text stepCount, span [] [ text stepUnit ] ]
            , label []
                [ text startBtnText
                , button
                    [ type_ "button"
                    , name "start"
                    , classList [ ( "active", model.gameActive ) ]
                    , onClick NewGame
                    ]
                    []
                ]
            , label []
                [ text "strict"
                , button
                    [ type_ "button"
                    , name "strict"
                    , classList [ ( "strict", model.strictMode ) ]
                    , onClick (ToggleStrict model.strictMode)
                    ]
                    []
                ]
            ]


sounds : List Sound -> Html Msg
sounds soundList =
    soundList
        |> List.map
            (\sound ->
                audio [ src sound.url, type_ "audio/mpeg", id ("sound" ++ toString sound.id), preload "auto" ] []
            )
        |> div [ id "media" ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    -- if model.gameActive && not model.allowInput then
    --     Sub.batch
    --         [ Time.every (250 * millisecond) Tick
    --         ]
    -- else
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
