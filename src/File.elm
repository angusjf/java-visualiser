module File exposing (..)

type alias Uri = String

type alias File =
  { uri : Uri
  , content : String
  }
