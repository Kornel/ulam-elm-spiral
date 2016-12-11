module Model exposing(..)

-- Move this!
type Msg = Resize Int Int | Move Int Int


type alias Model =
    { screen :
        { width : Int
        , height : Int
        },
      spiral :
        {  n : Int
         , wx : Int
         , wy : Int
        }
    }


init : Cmd Msg -> (Model, Cmd Msg)
init initMsg =
    ( { screen =
        {  width = 0
         , height = 0
        },
        spiral =
        {  n = 50
         , wx = 12
         , wy = 12
        }
      },
      initMsg
    )
