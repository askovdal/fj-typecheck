module TypeCheck.WFClass

open AST
open ClassTable
open TypeCheck.WFVar
open Utils

let typeArgumentsWithinBounds // Δ ⊢ T̄ <: [T̄/X̄]N̄
    (typeArguments: Type list) // T̄
    (classDef: Class) // class C<X̄ ◁ N̄> ◁ N {...}
    (typeEnv: TypeParameter list) // Δ
    (classTable: ClassTable)
    ()
    =
    let typeArgumentWithinBound (typeArgument: Type) (typeParameter: TypeParameter) () =
        match typeArgument with
        | TypeVariable typeVariableName ->
            let typeVariable =
                typeEnv |> List.find (fun typeVariable -> typeVariable.Name = typeVariableName)

            // Do substitution first:
            // Iterate through typeParameter.Bound
            // Every time we find a TypeVariable:
            // Look up which index the type variable has in classDef.TypeParameters
            // Replace the TypeVariable with the same index from typeArguments
            if typeVariable.Bound = typeParameter.Bound then
                //     Check if S-Var is applicable (the bound is the same as the type variable's bound)
                Ok()
            else
                //     Else run S-Trans
                Ok()

        | NonvariableType nonvariableType ->
            //     Check if S-Class is applicable
            //     Else run S-Trans
            Ok()

    let folder (state: Result<unit, string>) (typeArgument: Type) (typeParameter: TypeParameter) =
        state |> Result.bind (typeArgumentWithinBound typeArgument typeParameter)

    (Ok(), typeArguments, classDef.TypeParameters) |||> List.fold2 folder


let rec typeArgumentsOk // Δ ⊢ T̄ ok
    (typeArguments: Type list) // T̄
    (typeEnv: TypeParameter list) // Δ
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

and wfClass
    (nvType: NonvariableType) // C<T̄>
    (typeEnv: TypeParameter list) // Δ
    (classTable: ClassTable)
    =
    match classTable |> ClassTable.tryFind nvType.ClassName with
    | None -> Error $"Class '{nvType.ClassName |> classNameString}' not defined"
    | Some classDef ->
        typeArgumentsOk nvType.TypeArguments typeEnv classTable
        |> Result.bind (typeArgumentsWithinBounds nvType.TypeArguments classDef typeEnv classTable)
