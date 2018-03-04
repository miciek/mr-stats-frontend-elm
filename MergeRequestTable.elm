module MergeRequestTable exposing (Model, Msg, init, update, view, subscriptions)

import Html exposing (Html, div, table, h1, text, tbody, thead, tr, th, td, a)
import Html.Attributes exposing (style, class, href)
import Http
import Json.Decode as Decode exposing (Decoder, map3, map4, field, int, string, float)
import Time exposing (Time, second)
import List exposing (sortWith, map)


-- MODEL
type alias MergeRequest =
  { title : String
  , upvotes : Int
  , updated_at : String
  }

type alias MergeRequestStats =
  { mergeRequest : MergeRequest
  , timeToMerge : Float
  , commentsQty : Int
  , url : String
  }

type alias Model =
  { mrStats : List MergeRequestStats
  , lastRefreshTime : Time
  }

-- SUBSCRIPTIONS
subscriptions: Model -> Sub Msg
subscriptions model =
  Time.every 5000 Tick


-- UPDATE
type Msg
  = UpdateList (Result Http.Error (List MergeRequestStats))
  | Tick Time

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UpdateList (Ok list) ->
      let
        mrComparison a b =
          if a.mergeRequest.updated_at < b.mergeRequest.updated_at then
            GT
          else
            LT
        sortedList = sortWith mrComparison list
      in
        ({ model | mrStats = sortedList }, Cmd.none)

    UpdateList (Err err) ->
      ({ model |  mrStats = [{ mergeRequest = { title = "ERROR: " ++ (errorToString err), upvotes = 0, updated_at = "" }, timeToMerge = 0, commentsQty = 0, url = "" }] }, Cmd.none)

    Tick clockTime ->
      ({ model | lastRefreshTime = clockTime }, mergeRequestsFetchEffect)


-- INIT
init : (Model, Cmd Msg)
init = ({ mrStats = [], lastRefreshTime = 0 }, mergeRequestsFetchEffect)


-- VIEW
view : Model -> Html Msg
view model =
  div [ layoutStyle ]
      [ h1 [ centeredStyle ] [ text "MR✵Stats" ]
      , table
          [ class "hx-table"
          , centeredStyle
          ]
          [ thead [] [ tr [] headers ]
          , tbody [] (map statsView model.mrStats)
          ]
      ]

headers : List (Html Msg)
headers =
  ["title", "comments", "time to merge"]
    |> map text
    |> map (List.repeat 1)
    |> map (th [])

statsView : MergeRequestStats -> Html Msg
statsView stats =
  tr []
  [ td [] [ a [ href stats.url ] [ text stats.mergeRequest.title ] ]
  , td [] [ text (toString stats.commentsQty)]
  , td [] [ text (formatTimeToMerge stats.timeToMerge) ]
  ]

formatTimeToMerge : Float -> String
formatTimeToMerge ttm =
  let
    minutes = truncate(ttm / 60)
    hours = truncate(ttm / 3600)
    days = hours // 24
  in
    if days > 1 then
      toString days ++ " days"
    else if hours > 1 then
      toString hours ++ " hours"
    else if minutes > 1 then
      toString minutes ++ " minutes"
    else "instantly"

-- STYLES
layoutStyle : Html.Attribute Msg
layoutStyle =
  style
    [ ("display", "flex")
    , ("align-items", "center")
    , ("justify-content", "center")
    , ("flex-direction", "column")
    , ("height", "100%")
    ]

centeredStyle : Html.Attribute Msg
centeredStyle =
  style
    [ ("max-width", "90%") ]

-- HTTP Request
mergeRequestsFetchEffect: Cmd Msg
mergeRequestsFetchEffect =
  Http.send UpdateList (Http.get backendUrl decodeMRs)

errorToString : Http.Error -> String
errorToString e =
  case e of
    Http.Timeout -> "timeout"
    Http.NetworkError -> "network error"
    Http.BadStatus s -> "bad status - " ++ (toString s)
    Http.BadPayload c s -> "bad response - " ++ (toString c) ++ " - " ++ (toString s)
    Http.BadUrl s -> "bad url - " ++ s

backendUrl : String
backendUrl = "http://localhost:8080/mrs"

decodeMRs : Decode.Decoder (List MergeRequestStats)
decodeMRs =
  Decode.list mergeRequestStatsDecoder

mergeRequestStatsDecoder: Decoder MergeRequestStats
mergeRequestStatsDecoder =
  map4 MergeRequestStats
    (field "mergeRequest" mergeRequestDecoder)
    (field "timeToMerge" float)
    (field "commentsQty" int)
    (field "url" string)

mergeRequestDecoder: Decoder MergeRequest
mergeRequestDecoder =
  map3 MergeRequest
    (field "title" string)
    (field "upvotes" int)
    (field "updated_at" string)
