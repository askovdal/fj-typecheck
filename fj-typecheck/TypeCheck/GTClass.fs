module TypeCheck.GTClass

open AST
open ClassTable
open TypeCheck.WFObject
open TypeCheck.WFClass
open Utils

let superclassOk // X̄ <: N̄ ⊢ N ok
    (superclass: NonvariableType) // N
    (state: State)
    ()
    =
    wfObject superclass
    |> orElse (wfClass superclass state)
    |> prefixError $"Error in superclass '{superclass |> debugNvType}':"

let boundsOk // X̄ <: N̄ ⊢ N̄ ok
    (state: State)
    =
    let typeParameters, _, _ = state

    let boundOk // X̄ <: N̄ ⊢ N ok
        (typeParameter: TypeParameter) // N
        ()
        =
        wfObject typeParameter.Bound
        |> orElse (wfClass typeParameter.Bound state)
        |> prefixError $"Error in bound '{typeParameter.Bound |> debugNvType}':"

    let folder (state: Result<unit, string>) (typeParameter: TypeParameter) =
        state |> Result.bind (boundOk typeParameter)

    (Ok(), typeParameters) ||> List.fold folder

let gtClass (classDef: Class) (classTable: ClassTable) (expansive: bool) () =
    let state = (classDef.TypeParameters, classTable, expansive)
    let result = boundsOk state |> Result.bind (superclassOk classDef.Superclass state)

    result |> prefixError $"Error in class '{classDef.Name |> classNameString}':"
