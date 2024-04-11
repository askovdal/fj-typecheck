module TypeCheck.WFVar

open AST
open Utils

let wfVar (typeVariableName: TypeVariableName) (typeEnv: TypeParameter list) =
    match typeEnv |> List.tryFind (fun typeVariable -> typeVariable.Name = typeVariableName) with
    | None -> Error $"Type variable '{typeVariableName |> typeVariableNameString}' not defined"
    | Some _ -> Ok()
