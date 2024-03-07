module Thesis.ClassesTest

open System
open Thesis.AST
open Thesis.TypeCheck

let classAConstructor =
    { ClassName = TypeName "A"
      Fields = [] }

let classA =
    { ClassName = TypeName "A"
      SuperclassName = TypeName "Object"
      Fields = []
      Constructor = classAConstructor
      Methods = [] }

let classBConstructor =
    { ClassName = TypeName "B"
      Fields = [ (TypeName "A", FieldName "b_a1"); (TypeName "A", FieldName "b_a2") ] }

let classB =
    { ClassName = TypeName "B"
      SuperclassName = TypeName "Object"
      Fields = [ (TypeName "A", FieldName "b_a1"); (TypeName "A", FieldName "b_a2") ]
      Constructor = classBConstructor
      Methods = [] }

let classCConstructor =
    { ClassName = TypeName "A"
      Fields =
        [ (TypeName "A", FieldName "b_a1")
          (TypeName "A", FieldName "b_a2")
          (TypeName "A", FieldName "c_a1") ] }

let classC =
    { ClassName = TypeName "C"
      SuperclassName = TypeName "B"
      Fields = [ (TypeName "A", FieldName "c_a1") ]
      Constructor = classCConstructor
      Methods = [] }

let classTable =
    ClassTable.empty |> ClassTable.addClasses [ classB; classC; classA ]

let result = typeCheckClassTable classTable

match result with
| Ok _ -> printfn "Type check success"
| Error err -> printfn $"%A{err}"

let foo = 0
