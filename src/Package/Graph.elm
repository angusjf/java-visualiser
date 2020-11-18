module Package.Graph exposing (..)

import Graph

type alias PackageGraph = Graph.Graph Entity Link

type alias Vertex = Graph.Vertex Link

type Link = Extends | Implements | References

type alias Entity =
  { id : Graph.NodeId
  , name : String
  , publicMethods : List Method
  , publicAttributes : List Attribute
  , kind : Kind
  , access : Access
  , abstract : Bool
  , final : Bool
  , static : Bool
  }

type Kind = Class | Interface | Enum

type Access = Public | Protected | Private

type alias Method =
  { identifier : String
  }

type alias Attribute =
  { typeIdentifiers : List String
  , prettyTypeName : String
  , identifier : String
  , multiple : Bool
  }