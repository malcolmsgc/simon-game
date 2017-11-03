module SimonsGame exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


-- MODEL


type alias Model =
    { test : String
    , strictMode : Bool
    , sequence : List Sound
    }


type alias Sound =
    { id : String
    , soundUrl : String
    }


type alias Quad =
    { id : Int
    , sound : Sound
    }



-- UPDATE
-- VIEW
-- SUBSCRIPTIONS
-- PROGRAM
