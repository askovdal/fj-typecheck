module TypeCheck.Constructor

open AST
open ClassTable
open Utils

let constructorHasAllFields (classDef, classTable) allFields =
    let constructorMissingParameter field =
        classDef.Constructor.Parameters |> List.contains field |> not

    match
        allFields
        |> List.map fieldToParameter
        |> List.tryFind constructorMissingParameter
    with
    | None -> Ok(classDef, classTable, allFields)
    | Some { Name = ParameterName name
             Type = parameterType } ->
        Error $"Parameter '{name}' of type '{parameterType |> debugType}' missing in constructor"

let constructorOnlyHasDefinedFields (classDef, classTable, allFields) =
    let parameterNotInAllFields parameter =
        allFields |> List.contains (parameter |> parameterToField) |> not

    match classDef.Constructor.Parameters |> List.tryFind parameterNotInAllFields with
    | None -> Ok(classDef, classTable)
    | Some { Name = ParameterName name } -> Error $"Parameter '{name}' is not part of the object's instance variables"

let typeCheckConstructor ((classDef, classTable): State) =
    let rec collectFields (classDef: Class) : Result<Field list, string> =
        match classDef.Superclass with
        | { ClassName = ClassName "Object" } -> Ok classDef.Fields
        | { ClassName = superclassName } ->
            match classTable |> ClassTable.tryFind superclassName with
            | None ->
                Error
                    $"Superclass {superclassName |> classNameString} of class {classDef.Name |> classNameString} is undefined"
            | Some superclass -> collectFields superclass |> Result.map ((@) classDef.Fields)

    collectFields classDef
    |> Result.bind (constructorHasAllFields (classDef, classTable))
    |> Result.bind constructorOnlyHasDefinedFields
