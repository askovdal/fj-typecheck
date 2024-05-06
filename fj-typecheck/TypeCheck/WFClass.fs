module TypeCheck.WFClass

open AST
open ClassTable
open TypeCheck.WFVar
open TypeCheck.WFObject
open Utils
open SubtypeAssertion

let typeArgumentsRespectBounds // 𝚫 ⊢ T̄ <: [T̄/X̄]N̄
    (typeArguments: Type list) // T̄
    (classDef: Class) // class C<X̄ ◁ N̄> ◁ N {...}
    (typeEnv: TypeParameter list) // 𝚫
    (classTable: ClassTable)
    ()
    =
    let typeArgumentRespectsBound // 𝚫 ⊢ T <: [T̄/X̄]N
        (typeArgument: Type) // T
        (bound: NonvariableType) // N
        ()
        =
        bound
        |> substituteInNvType typeArguments classDef.TypeParameters // [T̄/X̄]N
        |> Result.bind (fun substitutedBound ->
            subtypeAssertion typeArgument (NonvariableType substitutedBound) typeEnv classTable
            |> prefixError
                $"Type argument '{typeArgument |> debugType}' does not respect its bound; should extend '{substitutedBound |> debugNvType}':")

    let folder (state: Result<unit, string>) (typeArgument: Type) (typeParameter: TypeParameter) =
        state
        |> Result.bind (typeArgumentRespectsBound typeArgument typeParameter.Bound)

    (Ok(), typeArguments, classDef.TypeParameters) |||> List.fold2 folder

let rec typeArgumentsOk // 𝚫 ⊢ T̄ ok
    (typeArguments: Type list) // T̄
    (typeEnv: TypeParameter list) // 𝚫
    (classTable: ClassTable)
    =
    let typeArgumentOk // 𝚫 ⊢ T ok
        (typeArgument: Type) // T
        ()
        =
        let result =
            match typeArgument with
            | TypeVariable typeVariableName -> wfVar typeVariableName typeEnv
            | NonvariableType nonvariableType ->
                wfObject nonvariableType |> orElse (wfClass nonvariableType typeEnv classTable)

        result |> prefixError $"Error in type argument '{typeArgument |> debugType}':"

    let folder (state: Result<unit, string>) (typeArgument: Type) =
        state |> Result.bind (typeArgumentOk typeArgument)

    (Ok(), typeArguments) ||> List.fold folder

and wfClass // 𝚫 ⊢ C<T̄> ok
    (nvType: NonvariableType) // C<T̄>
    (typeEnv: TypeParameter list) // 𝚫
    (classTable: ClassTable)
    ()
    =
    classTable
    |> ClassTable.find nvType.ClassName
    |> Result.bind (fun classDef -> // class C<X̄ ◁ N̄> ◁ N {...}
        typeArgumentsOk nvType.TypeArguments typeEnv classTable
        |> Result.bind (typeArgumentsRespectBounds nvType.TypeArguments classDef typeEnv classTable))
