module Graph exposing
 ( NodeId
 , Graph
 , Edge
 , Node
 , mkNodeId
 )

type alias Graph n v =
  { nodes : List (Node n)
  , edges : List (Edge v)
  }

type alias Edge a =
  { from : NodeId
  , to : NodeId
  , data : a
  }

type alias Node n = { n | id : NodeId }

type alias NodeId = String

mkNodeId : String -> String -> NodeId
mkNodeId pkg class = pkg ++ "." ++ class
