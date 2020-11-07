module Graph exposing (NodeId, Graph, Class, Vertex, mkNodeId, Kind(..))

type alias NodeId = String

type alias Vertex =
  { from : NodeId
  , to : NodeId
  }

type alias Graph =
  { classes : List Class
  , extensions : List Vertex
  , implements : List Vertex
  }

type Kind = Normal | Public

type alias Class =
  { id : NodeId
  , name : String
  , kind : Kind
  }

mkNodeId : String -> String -> NodeId
mkNodeId pkg class = pkg ++ "." ++ class
