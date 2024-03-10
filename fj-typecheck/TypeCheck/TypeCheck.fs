module TypeCheck.TypeCheck

open AST
open TypeCheck.ClassGenerics
open TypeCheck.Superclass
open TypeCheck.ClassFields
open TypeCheck.Constructor

let typeCheckClass ((classDef, classTable): State) =
    let result =
        Ok(classDef, classTable)
        |> Result.bind typeCheckClassGenerics
        |> Result.bind typeCheckSuperclass
        |> Result.bind typeCheckClassFields
        |> Result.bind typeCheckConstructor

    match result with
    | Ok(_, classTable) -> Ok classTable
    | Error err -> Error $"Error in class '{classDef.Name |> classNameString}': {err}"

let typeCheckClasses (state: Result<ClassTable, string>) (classDef: Class) =
    match state with
    | Ok classTable -> typeCheckClass (classDef, classTable)
    | Error err -> Error err

let typeCheckClassTable (classTable: ClassTable) =
    classTable
    |> Map.values
    |> Seq.toList
    |> List.fold typeCheckClasses (Ok classTable)
