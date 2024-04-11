module Utils

open AST

let isObject = (=) (ClassName "Object")

let classNameString (ClassName className) = className
let typeVariableNameString (TypeVariableName typeVariableName) = typeVariableName
let fieldNameString (FieldName fieldName) = fieldName
let parameterNameString (ParameterName parameterName) = parameterName

let parameterToField
    ({ Type = typeDef
       Name = ParameterName name }: Parameter)
    : Field =
    { Type = typeDef
      Name = FieldName name }

let fieldToParameter
    ({ Type = typeDef
       Name = FieldName name }: Field)
    : Parameter =
    { Type = typeDef
      Name = ParameterName name }

let rec debugType (typeDef: Type) =
    match typeDef with
    | TypeVariable(TypeVariableName name) -> name
    | NonvariableType { ClassName = ClassName name
                        TypeArguments = generics } ->
        let genericsString = generics |> List.map debugType |> String.concat ", "

        if genericsString = "" then
            name
        else
            $"{name}<{genericsString}>"

let prefixError prefixMsg =
    Result.mapError (fun errorValue -> $"{prefixMsg} {errorValue}")
