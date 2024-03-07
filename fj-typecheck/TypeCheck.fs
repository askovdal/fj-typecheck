module Thesis.TypeCheck

open Microsoft.FSharp.Core
open Thesis.AST

type ClassTable = Map<TypeName, Class>

let isObject = (=) (TypeName "Object")

module ClassTable =
    let empty: ClassTable = Map.empty

    let addClasses (classDefs: Class list) (classTable: ClassTable) =
        let addClass (classTable: ClassTable) (classDef: Class) =
            classTable |> Map.add classDef.ClassName classDef

        (classTable, classDefs) ||> List.fold addClass

    let containsClass (className: TypeName) (classTable: ClassTable) =
        className |> isObject || classTable |> Map.containsKey className

    let tryFind (className: TypeName) (classTable: ClassTable) : Class option =
        if className |> isObject then
            let objectClass =
                { ClassName = TypeName "Object"
                  Generics = []
                  SuperclassName = TypeName "Object"
                  Fields = []
                  Constructor =
                    { ClassName = TypeName "Object"
                      Fields = [] }
                  Methods = [] }

            Some objectClass
        else
            classTable |> Map.tryFind className

type State = Class * ClassTable

let fieldsDistinctFromSuperclass ((classDef, classTable): State) =
    match classTable |> ClassTable.tryFind classDef.SuperclassName with
    | None -> Error $"Superclass {typeString classDef.SuperclassName} undefined"
    | Some superclass ->
        let superclassHasField field =
            superclass.Fields |> List.map snd |> List.contains (snd field)

        match classDef.Fields |> List.tryFind superclassHasField with
        | None -> Ok(classDef, classTable)
        | Some field -> Error $"Field {fieldName field} already defined in superclass {typeString superclass.ClassName}"

// let fieldsTypesDefined ((classDef, classTable): State) =
//     let fieldTypeUndefined (typeName, _) =
//         classTable |> ClassTable.containsClass typeName |> not
//
//     match classDef.Fields |> List.tryFind fieldTypeUndefined with
//     | None -> Ok(classDef, classTable)
//     | Some undefinedField ->
//         Error $"Field with name {fieldName undefinedField} has undefined type {fieldType undefinedField}"

let typeCheckConstructor ((classDef, classTable): State) =
    let rec collectFields (classDef: Class) : Result<Field list, string> =
        match classDef.SuperclassName with
        | TypeName "Object" -> Ok classDef.Fields
        | superclassName ->
            match classTable |> ClassTable.tryFind superclassName with
            | None ->
                Error $"Superclass {typeString superclassName} in class {typeString classDef.ClassName} is undefined"
            | Some superclass -> collectFields superclass |> Result.map ((@) classDef.Fields)

    let constructorMissingField field =
        classDef.Constructor.Fields |> List.contains field |> not

    collectFields classDef
    |> Result.bind (fun allFields ->
        match allFields |> List.tryFind constructorMissingField with
        | None -> Ok(classDef, classTable)
        | Some missingField -> Error $"Field {fieldName missingField} missing in constructor")


let typeCheckClass ((classDef, classTable): State) =
    let result =
        Ok(classDef, classTable)
        |> Result.bind fieldsDistinctFromSuperclass
        // |> Result.bind fieldsTypesDefined
        |> Result.bind typeCheckConstructor

    match result with
    | Ok(_, classTable) -> Ok classTable
    | Error err -> Error $"Error in class {typeString classDef.ClassName}: {err}"

let typeCheckClasses (state: Result<ClassTable, string>) (classDef: Class) =
    match state with
    | Ok classTable -> typeCheckClass (classDef, classTable)
    | Error err -> Error err

let typeCheckClassTable (classTable: ClassTable) =
    classTable
    |> Map.values
    |> Seq.toList
    |> List.fold typeCheckClasses (Ok classTable)
