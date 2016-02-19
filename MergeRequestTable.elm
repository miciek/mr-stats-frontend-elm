module MergeRequestTable where

import Effects exposing (Effects)
import Html exposing (..)
import Html.Attributes exposing (style, class, href)
import Http exposing (get)
import Json.Decode as Decode exposing (Decoder, (:=))
import Task exposing (Task)
import Time exposing (Time, second)
import Signal exposing (Address)
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


-- UPDATE

type Action =
  UpdateList (List MergeRequestStats)
  | Tick Time

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    UpdateList list ->
      let
        mrComparison a b =
          if a.mergeRequest.updated_at < b.mergeRequest.updated_at then
            GT
          else
            LT
        sortedList = sortWith mrComparison list
      in
        ({ model | mrStats = sortedList }, Effects.tick Tick)
    Tick clockTime ->
      let
        refreshInterval = 5 * second
      in
        if clockTime - model.lastRefreshTime < refreshInterval then
          (model, Effects.tick Tick)
        else
          ({ model | lastRefreshTime = clockTime }, mergeRequestsFetchEffect)


-- INIT

init : (Model, Effects Action)
init = ({ mrStats = [], lastRefreshTime = 0 }, mergeRequestsFetchEffect)


-- VIEW

view : Address Action -> Model -> Html
view address model =
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

headers : List Html
headers =
  ["title", "comments", "time to merge"]
    |> map text
    |> map (List.repeat 1)
    |> map (th [])

statsView : MergeRequestStats -> Html
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

layoutStyle : Attribute
layoutStyle =
  style
    [ ("display", "flex")
    , ("align-items", "center")
    , ("justify-content", "center")
    , ("flex-direction", "column")
    , ("height", "100%")
    ]

centeredStyle : Attribute
centeredStyle =
  style
    [ ("max-width", "90%") ]


-- EFFECTS

mergeRequestsFetchEffect : Effects Action
mergeRequestsFetchEffect =
  Http.get decodeMRs backendUrl
      |> Task.toResult
      |> Task.map (\s -> mrFromResult s)
      |> Task.map (\mrs -> UpdateList mrs)
      |> Effects.task

mrFromResult : Result Http.Error (List MergeRequestStats) -> List MergeRequestStats
mrFromResult s = case s of
    Result.Err err -> [{ mergeRequest = { title = "ERROR: " ++ (errorToString err), upvotes = 0, updated_at = "" }, timeToMerge = 0, commentsQty = 0, url = "" }]
    Result.Ok mrs -> mrs

errorToString : Http.Error -> String
errorToString e = case e of
                    Http.Timeout -> "timeout"
                    Http.NetworkError -> "network error"
                    Http.UnexpectedPayload s -> "unexpected payload - " ++ s
                    Http.BadResponse c s -> "bad response - " ++ (toString c) ++ " - " ++ s 

backendUrl : String
backendUrl = "http://localhost:8080/mrs"

decodeMRs : Decoder (List MergeRequestStats)
decodeMRs = Decode.list
            <| let
                mrDecoder = Decode.object3 MergeRequest
                  ("title" := Decode.string)
                  ("upvotes" := Decode.int)
                  ("updated_at" := Decode.string)
               in Decode.object4 MergeRequestStats
                  ("mergeRequest" := mrDecoder)
                  ("timeToMerge" := Decode.float)
                  ("commentsQty" := Decode.int)
                  ("url" := Decode.string)
