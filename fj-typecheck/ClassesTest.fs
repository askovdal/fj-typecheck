module Thesis.ClassesTest

open System
open Thesis.AST
open Thesis.TypeCheck

let classAConstructor =
    { ClassName = TypeName "A"
      Fields = [] }

let classA =
    { ClassName = TypeName "A"
      Generics = []
      SuperclassName = TypeName "Object"
      Fields = []
      Constructor = classAConstructor
      Methods = [] }

let classBConstructor =
    { ClassName = TypeName "B"
      Fields = [ (boringType "A", FieldName "b_a1"); (boringType "A", FieldName "b_a2") ] }

let classB =
    { ClassName = TypeName "B"
      Generics = []
      SuperclassName = TypeName "Object"
      Fields = [ (boringType "A", FieldName "b_a1"); (boringType "A", FieldName "b_a2") ]
      Constructor = classBConstructor
      Methods = [] }

let classCConstructor =
    { ClassName = TypeName "A"
      Fields =
        [ (boringType "A", FieldName "b_a1")
          (boringType "A", FieldName "b_a2")
          (boringType "A", FieldName "c_a1") ] }

let classC =
    { ClassName = TypeName "C"
      Generics = []
      SuperclassName = TypeName "B"
      Fields = [ (boringType "A", FieldName "c_a1") ]
      Constructor = classCConstructor
      Methods = [] }

let classTable =
    ClassTable.empty |> ClassTable.addClasses [ classB; classC; classA ]

let result = typeCheckClassTable classTable

match result with
| Ok _ -> printfn "Type check success"
| Error err -> printfn $"%A{err}"

let foo = 0
