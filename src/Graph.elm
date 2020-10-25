module Graph exposing (NodeId, Graph, Class, mkNodeId)

type Msg
  = DragEvent
      { x : Float
      , y : Float
      , id : NodeId
      }

type NodeId = NodeId String

type alias Graph =
  { classes : List Class
  , extensions : List (NodeId, NodeId)
  }

type alias Class =
  { id : NodeId
  , name : String
  , public : Bool
  }

mkNodeId : String -> String -> NodeId
mkNodeId pkg class = NodeId <| pkg ++ "." ++ class

