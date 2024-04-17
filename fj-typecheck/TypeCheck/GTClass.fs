module TypeCheck.GTClass

open AST
open ClassTable
open TypeCheck.WFClass
open Utils

let superclassOk // X̄ <: N̄ ⊢ N ok
    (superclass: NonvariableType) // N
    (typeParameters: TypeParameter list) // X̄ <: N̄
    (classTable: ClassTable)
    ()
    =
    wfClass superclass typeParameters classTable
    |> prefixError $"Error in superclass '{superclass |> debugNvType}':"

let boundsOk // X̄ <: N̄ ⊢ N̄ ok
    (typeParameters: TypeParameter list) // X̄ <: N̄
    (classTable: ClassTable)
    =
    let boundOk (typeParameter: TypeParameter) () =
        wfClass typeParameter.Bound typeParameters classTable
        |> prefixError $"Error in bound '{typeParameter.Bound |> debugNvType}':"

    let folder (state: Result<unit, string>) (typeParameter: TypeParameter) =
        state |> Result.bind (boundOk typeParameter)

    (Ok(), typeParameters) ||> List.fold folder

let gtClass (classDef: Class) (classTable: ClassTable) () =
    let result =
        boundsOk classDef.TypeParameters classTable
        |> Result.bind (superclassOk classDef.Superclass classDef.TypeParameters classTable)

    result |> prefixError $"Error in class '{classDef.Name |> classNameString}':"
