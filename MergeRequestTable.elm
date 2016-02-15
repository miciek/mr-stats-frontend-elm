module MergeRequestTable where

import Effects exposing (Effects)
import Html exposing (..)
import Html.Attributes exposing (style, class, href)
import Http exposing (get)
import Json.Decode as Decode exposing (Decoder, (:=))
import Task exposing (Task)
import Time exposing (Time, second)


-- MODEL

type alias MergeRequest =
  { title : String
  , upvotes : Int
  }

type alias MergeRequestStats =
  { mergeRequest : MergeRequest
  , timeToMerge : Int
  , commentsQty : Int
  , url : String
  }

type alias Model =
  { mrStats : List MergeRequestStats
  , lastRefreshTime : Time
  }


-- UPDATE

type Action =
  UpdatedList (List MergeRequestStats)
  | Tick Time

update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    UpdatedList list ->
      ( { model | mrStats = list }, Effects.tick Tick )
    Tick clockTime ->
      let
        refreshInterval = 5 * second
      in
        if clockTime - model.lastRefreshTime < refreshInterval then
          ( model, Effects.tick Tick )
        else
          ( { model | lastRefreshTime = clockTime }, getMergeRequests )


-- INIT

init : (Model, Effects Action)
init = ( { mrStats = [], lastRefreshTime = 0 }, getMergeRequests)


-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  div
    [ layoutStyle ]
    [ h1 [ centeredStyle ] [ text "MR✵Stats" ]
    , table
        [ class "hx-table"
        , centeredStyle
        ]
        [ thead []
            [ tr [] headers ]
        , tbody []
            (model.mrStats |> List.map (\mr -> tr [] (viewMR mr) ))
        ]
    ]

headers : List Html
headers =
  ["title", "upvotes", "comments", "TTM"]
    |> List.map text
    |> List.map (List.repeat 1)
    |> List.map (th [])

viewMR : MergeRequestStats -> List Html
viewMR stats = [ td [] [ a [ href stats.url ] [ text stats.mergeRequest.title ] ]
               , td [] [ text (toString stats.mergeRequest.upvotes)]
               , td [] [ text (toString stats.commentsQty)]
               , td [] [ text (formatTimeToMerge stats.timeToMerge) ]
               ]

formatTimeToMerge : Int -> String
formatTimeToMerge ttm =
  let
    minutes = ttm // 60
    hours = ttm // 3600
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

getMergeRequests : Effects Action
getMergeRequests =
  Http.get decodeMRs backendUrl
      |> Task.toMaybe
      |> Task.map (\s -> mrFromMaybe s)
      |> Task.map (\mrs -> UpdatedList mrs)
      |> Effects.task

mrFromMaybe : Maybe (List MergeRequestStats) -> List MergeRequestStats
mrFromMaybe s =
  case s of
    Nothing -> [{ mergeRequest = { title = "ERROR", upvotes = 0 }, timeToMerge = 0, commentsQty = 0, url = "" }]
    Just mrs -> mrs

backendUrl : String
backendUrl = "http://localhost:8080/mrs"

decodeMRs : Decoder (List MergeRequestStats)
decodeMRs = Decode.list
            <| let
                mrDecoder = Decode.object2 MergeRequest
                  ("title" := Decode.string)
                  ("upvotes" := Decode.int)
               in Decode.object4 MergeRequestStats
                  ("mergeRequest" := mrDecoder)
                  ("timeToMerge" := Decode.int)
                  ("commentsQty" := Decode.int)
                  ("url" := Decode.string)


