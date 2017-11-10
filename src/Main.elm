port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, on)
import Random
import Time exposing (Time, millisecond, second)
import Task
import Process exposing (sleep)
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
    , sequence : Sequence
    , userSequence : Sequence
    , count : Maybe Int
    , delayFor : Maybe Time --might not be nec
    }


type alias Sequence =
    Array Int


emptyArray : Sequence
emptyArray =
    Array.fromList []


initModel : Model
initModel =
    { test = "This is working"
    , gameActive = False
    , allowInput = False
    , strictMode = False
    , gameSounds = gameSounds
    , sequence = emptyArray
    , userSequence = emptyArray
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
    = NewGame
    | UpdateCount
    | PopulateSequence (List Int)
    | ToggleStrict Bool
    | PlaySound Int Int
    | UserEntries Int
    | NextPlaybackDelay Int Float
    | TouchpadPress Int
    | SequenceController Int (Maybe Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewGame ->
            let
                currentCount =
                    Array.length model.userSequence

                newModel =
                    if currentCount > 0 then
                        { model | sequence = emptyArray, userSequence = emptyArray, count = Just 1, allowInput = False }
                    else
                        { model | gameActive = True, count = Just 1 }
            in
                ( newModel, generateSequence newModel )

        PopulateSequence sequenceList ->
            let
                sequenceArr =
                    Array.fromList sequenceList
            in
                -- playnext index model
                update (SequenceController 0 model.count) { model | sequence = sequenceArr }

        UpdateCount ->
            case model.count of
                Just int ->
                    let
                        newCount =
                            int + 1
                    in
                        -- need to update sequencecontroller's 1st arg
                        update (SequenceController 0 (Just newCount)) { model | count = Just newCount }

                Nothing ->
                    ( model, Cmd.none )

        ToggleStrict isStrict ->
            ( { model | strictMode = (not isStrict) }, Cmd.none )

        UserEntries id ->
            ( { model | userSequence = Array.push id model.userSequence }, Cmd.none )

        NextPlaybackDelay index delay ->
            -- Delete model update?
            ( { model | delayFor = (Just delay) }, playSequence index model.sequence (Just delay) )

        PlaySound id index ->
            let
                cmd =
                    -- Cmd.map (always (SequenceController (index + 1) model.count)) (playSound id)
                    (playSound id)
                    |> Cmd.map (always (SequenceController (index + 1) model.count))
            in
                ( model, cmd )

        TouchpadPress id ->
            let
                cmd =
                    if model.allowInput then
                        Cmd.none
                    else
                        Cmd.none

                newModel =
                    if model.allowInput then
                        { model | userSequence = Array.push id model.userSequence }
                    else
                        model
            in
                ( newModel, cmd )

        SequenceController nextIndex count ->
            let
                nextIndexAdjusted =
                    (log " nexti" nextIndex) + 1

                -- + 1 to adjust to match count's 1 based index
            in
                case count of
                    Just countInt ->
                        case (log "nextadjusted" nextIndexAdjusted <= log "count" countInt) of
                            True ->
                                ( model, generateDelay nextIndexAdjusted )

                            False ->
                                -- This is where cmd to open input to user will go
                                ( { model | allowInput = not model.allowInput }, Cmd.none )

                    Nothing ->
                        ( initModel, Cmd.none )



-- HELPER FUNCTIONS


setTimeout : Time -> Msg -> Cmd Msg
setTimeout time msg =
    Process.sleep time
        |> Task.perform (\_ -> msg)


countToIndex : Maybe Int -> Int
countToIndex count =
    let
        index =
            case count of
                Just 0 ->
                    0

                Just int ->
                    int - 1

                Nothing ->
                    -10
    in
        index



-- TASKS
-- Generates a List which is then converted to an Array in PopulateSequence


generateSequence : Model -> Cmd Msg
generateSequence { sequence, count } =
    if
        sequence
            |> Array.isEmpty
    then
        Random.list 20 (Random.int 1 4)
            |> Random.generate PopulateSequence
    else
        -- if sequence exists go straight to playback of first pattern
        generateDelay 0


generateDelay : Int -> Cmd Msg
generateDelay index =
    -- (NextPlaybackDelay index)
    -- |> (Random.float 0.5 1.5)
    -- |> Random.generate
    Random.generate (NextPlaybackDelay index) (Random.float 0.5 1.5)


playSequence : Int -> Sequence -> Maybe Time -> Cmd Msg
playSequence index sequence delay =
    let
        audioId =
            Array.get index sequence

        seconds =
            case delay of
                Just float ->
                    float * second

                Nothing ->
                    1 * second
    in
        case audioId of
            Just id ->
                PlaySound id index
                    |> setTimeout seconds

            Nothing ->
                Cmd.none



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
    [ div [ class "touchpad top-row", id "top-left", onClick (TouchpadPress 1) ] []
    , div [ class "touchpad top-row", id "top-right", onClick (TouchpadPress 2) ] []
    , controls model
    , div [ class "touchpad bottom-row", id "bottom-left", onClick (TouchpadPress 3) ] []
    , div [ class "touchpad bottom-row", id "bottom-right", onClick (TouchpadPress 4) ] []
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
