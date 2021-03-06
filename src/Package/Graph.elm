module Package.Graph exposing (..)

import Graph

type alias PackageGraph = Graph.Graph Entity Link

type alias Edge = Graph.Edge Link
type alias Node = Graph.Node Entity

type Link = Extends | Implements | References

type alias Entity =
  { pkg : String
  , name : String
  , publicMethods : List Method
  , publicAttributes : List Attribute
  , kind : Kind
  , access : Access
  , abstract : Bool
  , final : Bool
  , static : Bool
  , expansion : Expansion
  , complexity : Float
  }

type Expansion = Not | Attrs | Stats

type Kind = Class | Interface | Enum

type Access = Public | Protected | Private

type alias Method =
  { identifier : String
  , numberOfParams : Int
  }

type alias Attribute =
  { typeIdentifiers : List String
  , prettyTypeName : String
  , identifier : String
  , multiple : Bool
  }
