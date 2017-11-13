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
    , activePad : PadRecord
    , gameSounds : List Sound
    , sequence : Sequence
    , userSequence : Sequence
    , correctSeq : Bool
    , showErr : Bool
    , count : Maybe Int
    , seqIndex : Int
    , delayFor : Maybe Time --might not be nec
    }


type alias Sequence =
    Array Int


type alias PadRecord =
    { topleft : Bool
    , topright : Bool
    , bottomleft : Bool
    , bottomright : Bool
    }


emptyArray : Sequence
emptyArray =
    Array.fromList []


initPads : PadRecord
initPads =
    { topleft = False
    , topright = False
    , bottomleft = False
    , bottomright = False
    }


initModel : Model
initModel =
    { test = "This is working"
    , gameActive = False
    , allowInput = False
    , strictMode = False
    , activePad = initPads
    , gameSounds = gameSounds
    , sequence = emptyArray
    , userSequence = emptyArray
    , correctSeq = True
    , showErr = False
    , count = Nothing
    , seqIndex = 0
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
    | PlaySound Int
    | NextPlaybackDelay Float
    | TouchpadPress Int
    | ValidateUserSequence Int
    | RemoveActiveClass Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewGame ->
            let
                currentCount =
                    Array.length model.userSequence

                newModel =
                    if currentCount > 0 then
                        { model
                            | sequence = emptyArray
                            , userSequence = emptyArray
                            , count = Just 2
                            , allowInput = False
                            , correctSeq = True
                            , seqIndex = 0
                        }
                    else
                        { model | gameActive = True, count = Just 2, seqIndex = 0 }
            in
                ( newModel, generateSequence newModel )

        PopulateSequence sequenceList ->
            let
                sequenceArr =
                    Array.fromList sequenceList
            in
                ( { model | sequence = sequenceArr }, sequenceControllerCmd model 0 model.count )

        UpdateCount ->
            case model.count of
                Just int ->
                    let
                        newCount =
                            int + 1
                    in
                        -- need to update sequencecontroller's 1st arg
                        ( { model | count = Just newCount }, sequenceControllerCmd model 0 (Just newCount) )

                Nothing ->
                    ( model, Cmd.none )

        ToggleStrict isStrict ->
            ( { model | strictMode = (not isStrict) }, Cmd.none )

        ValidateUserSequence id ->
            let
                isMatch =
                    validateSequence model
            in
                if log "isMatch: " isMatch then
                    update (PlaySound id) model
                else
                    { model
                        | correctSeq = False
                        , showErr = True
                        , userSequence = emptyArray
                        , seqIndex = 0
                    }
                        ! [ msgAsCmd (PlaySound id) ]

        RemoveActiveClass id ->
            let
                -- activePad =
                --     model.activePad
                newActivePad =
                    idToPad model.activePad id False
            in
                ( { model | activePad = newActivePad }, Cmd.none )

        NextPlaybackDelay delay ->
            
                ( model, playSequence model.seqIndex model.sequence (Just delay) )

        PlaySound id ->
            -- let
            --     cmd =
            --         (Cmd.none)
            --         |> Cmd.map (always (SequenceController (index + 1) model.count))
            -- in
            let
                newActivePad =
                    idToPad model.activePad id True

                removeActive =
                    setTimeout (350 * millisecond) (RemoveActiveClass id)

                showError =
                    not model.correctSeq

                nextIndex =
                    if model.correctSeq then
                        model.seqIndex + 1
                    else
                        0

                cmdBatch =
                    [ (playSound id), removeActive, (sequenceControllerCmd model nextIndex model.count) ]
            in
                if (openUserInput model) then
                    { model | allowInput = True, activePad = newActivePad } ! cmdBatch
                else
                    { model
                        | seqIndex = nextIndex
                        , allowInput = False
                        , activePad = newActivePad
                        , correctSeq = True
                        , showErr = showError
                    }
                        ! cmdBatch

        TouchpadPress id ->
            let
                newModel =
                    { model | userSequence = Array.push id model.userSequence }
            in
                if model.allowInput then
                    update (ValidateUserSequence id) newModel
                else
                    ( model, Cmd.none )



-- HELPER FUNCTIONS


msgAsCmd : msg -> Cmd msg
msgAsCmd msg =
    Task.succeed msg
        |> Task.perform identity


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


idToPad : PadRecord -> Int -> Bool -> PadRecord
idToPad padrecord id switchTo =
    case id of
        1 ->
            { padrecord | topleft = switchTo }

        2 ->
            { padrecord | topright = switchTo }

        3 ->
            { padrecord | bottomleft = switchTo }

        4 ->
            { padrecord | bottomright = switchTo }

        _ ->
            padrecord


validateSequence : Model -> Bool
validateSequence { count, sequence, userSequence } =
    let
        testToIndex =
            (Array.length userSequence)

        --not zero adjusted as slice omits end index
        seqToMatch =
            Array.slice 0 testToIndex sequence

        testFunc index inputValue =
            case (Array.get index seqToMatch) of
                Just val ->
                    inputValue == val

                Nothing ->
                    False

        validate =
            Array.indexedMap testFunc userSequence
    in
        log "validate" (Array.foldr (\a b -> a && b) True validate)



-- TASKS
-- Generates a List which is then converted to an Array in PopulateSequence


generateSequence : Model -> Cmd Msg
generateSequence { sequence } =
    if
        sequence
            |> Array.isEmpty
    then
        Random.list 20 (Random.int 1 4)
            |> Random.generate PopulateSequence
    else
        -- if sequence exists go straight to playback of first pattern
        generateDelay


generateDelay : Cmd Msg
generateDelay =
    -- (NextPlaybackDelay index)
    -- |> (Random.float 0.5 1.5)
    -- |> Random.generate
    -- NB needs min delay of 0.7secs to allow precceding sound to finish
    Random.generate NextPlaybackDelay (Random.float 0.8 1.5)


playSequence : Int -> Sequence -> Maybe Time -> Cmd Msg
playSequence index sequence delay =
    let
        audioId =
            -- if model.correctSeq then
            Array.get index sequence

        -- else
        --     Nothing
        seconds =
            case delay of
                Just float ->
                    float * second

                Nothing ->
                    1 * second
    in
        case audioId of
            Just id ->
                PlaySound id
                    |> setTimeout seconds

            Nothing ->
                Cmd.none


openUserInput : Model -> Bool
openUserInput { seqIndex, count, correctSeq } =
    let
        indexAdjusted =
            (log "nexti-Model" seqIndex) + 1

        -- + 1 to adjust to match count's 1 based index
    in
        if correctSeq then
            case count of
                Just countInt ->
                    log "nextadjusted-Model" indexAdjusted >= log "count-Model" countInt

                -- return True when adjusted index equals current step count
                Nothing ->
                    False
        else
            --return false when user has input incorrect sequence
            False


sequenceControllerCmd : Model -> Int -> Maybe Int -> Cmd Msg
sequenceControllerCmd { correctSeq, strictMode, sequence } index count =
    let
        indexAdjusted =
            (log "nexti-CMD" index) + 1

        -- + 1 to adjust to match count's 1 based index
    in
        if log "sequenceControllerCmd correctSeq" correctSeq then
            case count of
                Just countInt ->
                    case (log "nextadjusted-CMD" indexAdjusted <= log "count-CMD" countInt) of
                        True ->
                            generateDelay

                        False ->
                            Cmd.none

                Nothing ->
                    Cmd.none
        else
            playSequence index sequence (Just 1.5)



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
    let
        active =
            model.activePad
    in
        [ div [ classList [ ( "touchpad", True ), ( "active", active.topleft ) ], id "top-left", onClick (TouchpadPress 1) ] []
        , div [ classList [ ( "touchpad", True ), ( "active", active.topright ) ], id "top-right", onClick (TouchpadPress 2) ] []
        , controls model
        , div [ classList [ ( "touchpad", True ), ( "active", active.bottomleft ) ], id "bottom-left", onClick (TouchpadPress 3) ] []
        , div [ classList [ ( "touchpad", True ), ( "active", active.bottomright ) ], id "bottom-right", onClick (TouchpadPress 4) ] []
        ]


controls : Model -> Html Msg
controls { count, gameActive, strictMode, showErr } =
    let
        stepCount =
            case count of
                Just int ->
                    if showErr then
                        "X"
                    else
                        (toString int)

                Nothing ->
                    "--"

        stepUnit =
            case count of
                Just int ->
                    if showErr then
                        "incorrect"
                    else if int <= 1 then
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
