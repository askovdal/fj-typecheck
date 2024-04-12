module TypeCheck.WFClass

open AST
open ClassTable
open TypeCheck.WFVar
open Utils
open STrans

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
            match typeArgument with
            | TypeVariable typeArgumentVariableName ->
                let typeArgumentVariable =
                    typeEnv
                    |> List.find (fun typeVariable -> typeVariable.Name = typeArgumentVariableName)

                // Check if S-Var is applicable (the substituted bound is equal to the type variable's bound)
                if typeArgumentVariable.Bound = substitutedBound then
                    Ok()
                else
                    sTrans
                        (TypeVariable typeArgumentVariableName)
                        (NonvariableType substitutedBound)
                        typeEnv
                        classTable
                    |> Result.bind (
                        optionOkOr
                            $"Type argument '{typeArgumentVariableName |> typeVariableNameString}' does not respect its bound; should extend '{substitutedBound |> debugNvType}'"
                    )

            | NonvariableType nonvariableType ->
                //     Check if S-Class is applicable
                //     Else run S-Trans
                Ok())

    let folder (state: Result<unit, string>) (typeArgument: Type) (typeParameter: TypeParameter) =
        state |> Result.bind (typeArgumentRespectsBound typeArgument typeParameter.Bound)

    (Ok(), typeArguments, classDef.TypeParameters) |||> List.fold2 folder

let rec typeArgumentsOk // 𝚫 ⊢ T̄ ok
    (typeArguments: Type list) // T̄
    (typeEnv: TypeParameter list) // 𝚫
    (classTable: ClassTable)
    =
    let typeArgumentOk (typeArgument: Type) () =
        let result =
            match typeArgument with
            | TypeVariable typeVariableName -> wfVar typeVariableName typeEnv
            | NonvariableType nonvariableType -> wfClass nonvariableType typeEnv classTable

        result |> prefixError $"Error in type argument '{typeArgument |> debugType}':"

    let folder (state: Result<unit, string>) (typeArgument: Type) =
        state |> Result.bind (typeArgumentOk typeArgument)

    (Ok(), typeArguments) ||> List.fold folder

and wfClass // 𝚫 ⊢ C<T̄> ok
    (nvType: NonvariableType) // C<T̄>
    (typeEnv: TypeParameter list) // 𝚫
    (classTable: ClassTable)
    =
    match classTable |> ClassTable.tryFind nvType.ClassName with
    | None -> Error $"Class '{nvType.ClassName |> classNameString}' not defined"
    | Some classDef ->
        typeArgumentsOk nvType.TypeArguments typeEnv classTable
        |> Result.bind (typeArgumentsRespectBounds nvType.TypeArguments classDef typeEnv classTable)
