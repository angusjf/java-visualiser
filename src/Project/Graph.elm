module Project.Graph exposing (..)

import Graph

type alias ProjectGraph = Graph.Graph Entity Link

type alias Edge = Graph.Edge Link
type alias Node = Graph.Node Entity

type Link = Link

type alias Entity =
  { name : String
  , expanded : Bool
  , kind : Kind
  }

type Kind = Class | Package

