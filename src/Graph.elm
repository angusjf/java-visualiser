module Graph exposing
 ( NodeId
 , Graph
 , Entity
 , Vertex
 , mkNodeId
 , Kind(..)
 , Access (..)
 , Method
 , Attribute
 )

type alias NodeId = String

type alias Graph =
  { entities : List Entity
  , extensions : List Vertex
  , implements : List Vertex
  , references : List Vertex
  }

type Kind = Class | Interface | Enum

type Access = Public | Protected | Private

type alias Entity =
  { id : NodeId
  , name : String
  , publicMethods : List Method
  , publicAttributes : List Attribute
  , kind : Kind
  , access : Access
  , abstract : Bool
  , final : Bool
  , static : Bool
  }

type alias Vertex =
  { from : NodeId
  , to : NodeId
  }

type alias Method =
  { identifier : String
  }

type alias Attribute =
  { typeIdentifiers : List String
  , prettyTypeName : String
  , identifier : String
  , multiple : Bool
  }

mkNodeId : String -> String -> NodeId
mkNodeId pkg class = pkg ++ "." ++ class
