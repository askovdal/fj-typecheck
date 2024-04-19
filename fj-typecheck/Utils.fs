module Utils

open AST

/// Transforms an Option<'T> into a Result<'T, 'TError>, mapping Some(v) to Ok(v) and None to Error(err).
let optionOkOr errorValue =
    function
    | Some value -> Ok value
    | None -> Error errorValue

/// Transforms a Result<'T, 'TError> list into a Result<'T list, 'TError>.
/// If every element in `results` is Ok(v), returns the list of values.
/// If one element in `results` is Error(err), returns the error.
let collectResults results =
    let rec collector acc =
        function
        | [] -> Ok(List.rev acc)
        | Ok v :: rest -> collector (v :: acc) rest
        | Error err :: _ -> Error err

    collector [] results

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

let rec debugNvType
    ({ ClassName = ClassName name
       TypeArguments = generics }: NonvariableType)
    =
    let genericsString = generics |> List.map debugType |> String.concat ", "

    if genericsString = "" then
        name
    else
        $"{name}<{genericsString}>"

and debugType (typeDef: Type) =
    match typeDef with
    | TypeVariable(TypeVariableName name) -> name
    | NonvariableType nvType -> debugNvType nvType

let prefixError prefixMsg =
    Result.mapError (fun errorValue -> $"{prefixMsg} {errorValue}")

let rec substituteTypeArgsForVars
    (typeArguments: Type list) // T̄
    (typeParameters: TypeParameter list) // X̄
    =
    function
    // Recursive step: Find any type variables in the nonvariable type's type arguments, and replace them
    // with the corresponding type arguments from the context.
    | NonvariableType nonvariableType ->
        nonvariableType.TypeArguments
        |> List.map (substituteTypeArgsForVars typeArguments typeParameters)
        |> collectResults
        |> Result.map (fun substitutedTypeArguments ->
            NonvariableType
                { nonvariableType with
                    TypeArguments = substitutedTypeArguments })

    // Substitution step: Replace the type variable with the corresponding type argument from the context.
    | TypeVariable typeVariableName ->
        match typeParameters |> List.tryFindIndex (fun tp -> tp.Name = typeVariableName) with
        | None -> Error $"Type variable '{typeVariableName |> typeVariableNameString}' not defined"
        | Some typeVariableIndex -> Ok(typeArguments |> List.item typeVariableIndex)

/// Takes a nonvariable type N and a set of type parameters and their corresponding type arguments.
/// Returns N, where all type parameters have been replaced by their corresponding type arguments.
let substituteInNvType // [T̄/X̄]N
    (typeArguments: Type list) // T̄
    (typeParameters: TypeParameter list) // X̄
    (bound: NonvariableType) // N
    =
    match NonvariableType bound |> substituteTypeArgsForVars typeArguments typeParameters with
    | Error errorValue -> Error $"Error when substituting in '{bound |> debugNvType}': {errorValue}"
    | Ok(TypeVariable typeVariableName) ->
        Error
            $"Unexpected error when substituting in '{bound |> debugNvType}': received type variable '{typeVariableName |> typeVariableNameString}'"

    | Ok(NonvariableType substitutedSuperclass) -> Ok substitutedSuperclass

let okOr op =
    function
    | Ok resultValue -> Ok resultValue
    | Error _ -> op ()
