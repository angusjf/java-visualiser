module Graph exposing (NodeId, Graph, Edge, Node)

type alias NodeId = String

type alias Graph n v =
  { nodes : List (Node n)
  , edges : List (Edge v)
  }

type alias Edge a =
  { from : NodeId
  , to : NodeId
  , data : a
  }

type alias Node n =
  { id : NodeId
  , data : n
  }
