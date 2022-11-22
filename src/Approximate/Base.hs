module Approximate.Base where

type Height = Int
type Width = Int
type HW = (Height, Width)

type ParameterTree = ([HW], Int)

type ParameterCNF = ((ParameterTree, Int), (Int, Int))
