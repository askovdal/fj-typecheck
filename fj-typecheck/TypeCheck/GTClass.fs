module TypeCheck.GTClass

open AST
open ClassTable
open TypeCheck.WFClass
open Utils

let boundsOk (typeParameters: TypeParameter list) (classTable: ClassTable) =
    let boundOk (typeParameter: TypeParameter) () =
        wfClass typeParameter.Bound typeParameters classTable
        |> prefixError $"Error in bound '{NonvariableType(typeParameter.Bound) |> debugType}':"

    let folder (state: Result<unit, string>) (typeParameter: TypeParameter) =
        state |> Result.bind (boundOk typeParameter)

    (Ok(), typeParameters) ||> List.fold folder

let gtClass (classDef: Class) (classTable: ClassTable) () =
    let result = boundsOk classDef.TypeParameters classTable

    result |> prefixError $"Error in class '{classDef.Name |> classNameString}':"
