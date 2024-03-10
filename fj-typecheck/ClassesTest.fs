module Thesis.ClassesTest

open Thesis.AST
open Thesis.TypeCheck

let objectType =
    { ClassName = TypeName "Object"
      Generics = [] }

let classAConstructor = { Fields = [] }

let classA =
    { ClassName = TypeName "A"
      Generics = []
      Superclass = objectType
      Fields = []
      Constructor = classAConstructor
      Methods = [] }

let classBConstructor = { Fields = [] }

let classB =
    { ClassName = TypeName "B"
      Generics = []
      Superclass = objectType
      Fields = []
      Constructor = classBConstructor
      Methods = [] }

let classPairConstructor =
    { Fields =
        [ (TypeVariable(TypeVariableName "X"), FieldName "fst")
          (TypeVariable(TypeVariableName "Y"), FieldName "snd") ] }

let pairZY =
    { ClassName = TypeName "Pair"
      Generics = [ TypeVariable(TypeVariableName "Z"); TypeVariable(TypeVariableName "Y") ] }

let setFst =
    { Generics =
        [ { Name = TypeVariableName "Z"
            Bound = objectType } ]
      ReturnType = NonvariableType pairZY
      MethodName = MethodName "setfst"
      Parameters = [ (TypeVariable(TypeVariableName "Z"), VariableName "newfst") ]
      Return =
        NewInstance(
            pairZY,
            [ Var(VariableName "newfst")
              FieldAccess(Var(VariableName "this"), FieldName "snd") ]
        ) }

let classPair =
    { ClassName = TypeName "Pair"
      Generics =
        [ { Name = TypeVariableName "X"
            Bound = objectType }
          { Name = TypeVariableName "Y"
            Bound =
              { ClassName = TypeName "Objecta"
                Generics = [] } } ]
      Superclass = objectType
      Fields =
        [ (TypeVariable(TypeVariableName "X"), FieldName "fst")
          (TypeVariable(TypeVariableName "Y"), FieldName "snd") ]
      Constructor = classPairConstructor
      Methods = [] }

let classTable =
    ClassTable.empty |> ClassTable.addClasses [ classA; classB; classPair ]

let result = typeCheckClassTable classTable

match result with
| Ok _ -> printfn "Type check success"
| Error err -> printfn $"%A{err}"

let foo = 0
