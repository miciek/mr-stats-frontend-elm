module Main exposing (..)

import MergeRequestTable exposing (Model, Msg, init, update, view, subscriptions)
import Html exposing (Html)

main: Program Never Model Msg
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
