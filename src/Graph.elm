module Graph exposing
 ( NodeId
 , Graph
 , Vertex
 , Node
 , mkNodeId
 )

type alias Graph n v =
  { nodes : List (Node n)
  , vertices : List (Vertex v)
  }

type alias Vertex a =
  { from : NodeId
  , to : NodeId
  , data : a
  }

type alias Node n = { n | id : NodeId }

type alias NodeId = String

mkNodeId : String -> String -> NodeId
mkNodeId pkg class = pkg ++ "." ++ class
