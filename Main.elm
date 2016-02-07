import StartApp exposing (App, start)
import MergeRequestTable exposing (Model, init, update, view)
import Effects exposing (Never)
import Task exposing (Task)
import Html exposing (Html)

app : App Model
app = start
       { init = init
       , inputs = []
       , update = update
       , view = view
       }

main : Signal Html
main = app.html

port tasks : Signal (Task Never ())
port tasks = app.tasks
