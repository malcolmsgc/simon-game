port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, on)
import Random
import Time exposing (Time, millisecond)
import Task 
import Array exposing (Array)
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
    , count : Maybe Int
    , delayFor : Maybe Time
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
    , count = Nothing
    , delayFor = Nothing
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
    ( initModel, Cmd.none )



-- UPDATE


type Msg
    = None
    | NewGame
    | UpdateCount
    | PopulateSequence (List Int)
    | ToggleStrict Bool
    | StaggerSound
    | PlaySound Int
    | UserEntries Int
    | NextPlaybackDelay Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        None ->
            ( model, Cmd.none )

        NewGame ->
            let
                currentCount =
                    List.length model.userSequence

                newModel =
                    if currentCount > 0 then
                        { model | sequence = [], userSequence = [], count = Just 1, allowInput = False }
                    else
                        { model | gameActive = True, count = Just 1 }
            in
                ( newModel, generateSequence newModel )

        PopulateSequence sequenceList ->
                ( { model | sequence = sequenceList }, startSequence )

        UpdateCount ->
            case model.count of
                Just int ->
                    let
                        newCount =
                            int + 1
                    in
                        ( { model | count = Just newCount }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        ToggleStrict isStrict ->
            ( { model | strictMode = (not isStrict) }, Cmd.none )

        UserEntries id ->
            ( { model | userSequence = model.userSequence ++ [ id ] }, Cmd.none )

        StaggerSound ->
            ( model, Random.generate NextPlaybackDelay (Random.float 0 1.5) )

        NextPlaybackDelay delay ->
            ( { model | delayFor = (Just delay) }, Cmd.none )

        PlaySound id ->
            ( model, Cmd.none )



-- TASKS


generateSequence : Model -> Cmd Msg
generateSequence { sequence, count } =
    if
        sequence
            |> List.isEmpty
    then
        Random.list 20 (Random.int 1 4)
            |> Random.generate PopulateSequence
    else
        -- if sequence exists go straight to playback of first pattern
        startSequence count sequence


startSequence : Maybe Int -> List Int -> Cmd Msg
startSequence count sequence =
    -- TO DO : Only have one id here as a test. Needs more logic to play full pattern
    let
        currentId = 
            List.take count sequence
    in
        
    playSound 1


startGame : Bool -> Cmd Msg
startGame firstRun =
    Cmd.none



--     let
--         cmds =
--             if firstRun then
--                 startSequence
--             else
--                 Task.succeed generateSequence
--                     |> Task.perform startSequence
--     in
--         cmds
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
controls { count, gameActive, strictMode } =
    let
        stepCount =
            case count of
                Just int ->
                    (toString int)

                Nothing ->
                    "--"

        stepUnit =
            case count of
                Just int ->
                    if int <= 1 then
                        "step"
                    else
                        "steps"

                Nothing ->
                    "press start"

        startBtnText =
            case gameActive of
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
                    , classList [ ( "active", gameActive ) ]
                    , onClick NewGame
                    ]
                    []
                ]
            , label []
                [ text "strict"
                , button
                    [ type_ "button"
                    , name "strict"
                    , classList [ ( "strict", strictMode ) ]
                    , onClick (ToggleStrict strictMode)
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



-- PORTS
-- pass id through to JS, which will be used to form the node id, which can be used to select the node


port playSound : Int -> Cmd msg



-- PROGRAM


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
