module ParserDemo exposing (..)

import Browser
import Java15Parser
import Element exposing (Element)
import Element.Input

type Msg = TextChanged String | Parse | Nah

type alias Model =
    { text : String
    , out : Result String Java15Parser.CompilationUnit
    }

init : () -> (Model, Cmd Msg)
init _ =
        ( { text = ""
          , out = Err "nothing yet"
          }
        , Cmd.none
        )

view : Model -> Browser.Document Msg
view model =
  { title = "Demo"
  , body = [ Element.layout [] (viewBody model) ]
  }

viewBody : Model -> Element Msg
viewBody model =
  Element.column 
    [ Element.width (Element.px 1000) ]
    [ Element.Input.multiline
        []
        { onChange = TextChanged
        , text = model.text
        , placeholder = Nothing
        , label = Element.Input.labelHidden "code"
        , spellcheck = False
        }
    , Element.Input.button [] 
      { onPress = Just Parse
      , label = Element.text "parse"
      }
    , Element.Input.multiline
        []
        { onChange = \_ -> Nah
        , text = Debug.toString model.out
        , placeholder = Nothing
        , label = Element.Input.labelHidden "code"
        , spellcheck = False
        }
    ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        TextChanged s -> ({ model | text = s }, Cmd.none)
        Parse ->
            ({ model
                | out = Java15Parser.parse Java15Parser.compilationUnit model.text
             }
            , Cmd.none
            )
        Nah -> (model, Cmd.none)

main =
  Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }
