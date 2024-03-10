module ClassesTest

open AST
open TypeCheck.TypeCheck

let objectType =
    { ClassName = ClassName "Object"
      Generics = [] }

let classAConstructor = { Parameters = [] }

let classA =
    { Name = ClassName "A"
      Generics = []
      Superclass = objectType
      Fields = []
      Constructor = classAConstructor
      Methods = [] }

let classBConstructor = { Parameters = [] }

let classB =
    { Name = ClassName "B"
      Generics = []
      Superclass = objectType
      Fields = []
      Constructor = classBConstructor
      Methods = [] }

let classPairConstructor =
    { Parameters =
        [ { Type = TypeVariable(TypeVariableName "X")
            Name = ParameterName "fst" }
          { Type = TypeVariable(TypeVariableName "Y")
            Name = ParameterName "snd" } ] }

let pairZY =
    { ClassName = ClassName "Pair"
      Generics = [ TypeVariable(TypeVariableName "Z"); TypeVariable(TypeVariableName "Y") ] }

let setFst =
    { Generics =
        [ { Name = TypeVariableName "Z"
            Bound = objectType } ]
      ReturnType = NonvariableType pairZY
      Name = MethodName "setfst"
      Parameters =
        [ { Type = TypeVariable(TypeVariableName "Z")
            Name = ParameterName "newfst" } ]
      Return =
        NewInstance
            { Class = pairZY
              Parameters =
                [ Variable "newfst"
                  FieldAccess
                      { Object = Variable "this"
                        Field = FieldName "snd" } ] } }

let classPair =
    { Name = ClassName "Pair"
      Generics =
        [ { Name = TypeVariableName "X"
            Bound = objectType }
          { Name = TypeVariableName "Y"
            Bound = objectType } ]
      Superclass = objectType
      Fields =
        [ { Type = TypeVariable(TypeVariableName "X")
            Name = FieldName "fst" }
          { Type = TypeVariable(TypeVariableName "Y")
            Name = FieldName "snd" } ]
      Constructor = classPairConstructor
      Methods = [ setFst ] }

let classTable =
    ClassTable.empty |> ClassTable.addClasses [ classA; classB; classPair ]

let result = typeCheckClassTable classTable

match result with
| Ok _ -> printfn "Type check success"
| Error err -> printfn $"%A{err}"

let foo = 0
