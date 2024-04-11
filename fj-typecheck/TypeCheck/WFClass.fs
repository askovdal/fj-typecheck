module TypeCheck.WFClass

open AST
open ClassTable
open TypeCheck.WFVar
open Utils

let typeArgumentsRespectBounds (typeEnv: TypeParameter list) (classTable: ClassTable) () =
    let typeArgumentRespectsBound (typeArgument: Type) =
        match typeArgument with
        | TypeVariable typeVariableName ->
            //     Check if S-Var is applicable (the bound is the same as the type variable's bound)
            //     Else run S-Trans
            Ok()
        | NonvariableType nonvariableType ->
            //     Check if S-Class is applicable
            //     Else run S-Trans
            Ok()

    Ok()

let rec typeArgumentsOk (typeArguments: Type list) (typeEnv: TypeParameter list) (classTable: ClassTable) =
    let typeArgumentOk (typeArgument: Type) () =
        let result =
            match typeArgument with
            | TypeVariable typeVariableName -> wfVar typeVariableName typeEnv
            | NonvariableType nonvariableType -> wfClass nonvariableType typeEnv classTable

        result |> prefixError $"Error in type argument '{typeArgument |> debugType}':"

    let folder (state: Result<unit, string>) (typeArgument: Type) =
        state |> Result.bind (typeArgumentOk typeArgument)

    (Ok(), typeArguments) ||> List.fold folder

and wfClass (nvType: NonvariableType) (typeEnv: TypeParameter list) (classTable: ClassTable) =
    match classTable |> ClassTable.tryFind nvType.ClassName with
    | None -> Error $"Class '{nvType.ClassName |> classNameString}' not defined"
    | Some classDef ->
        typeArgumentsOk nvType.TypeArguments typeEnv classTable
        |> Result.bind (typeArgumentsRespectBounds typeEnv classTable)
