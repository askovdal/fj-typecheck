module TypeCheck.SVar

open AST
open Utils

let sVar // 𝚫 ⊢ X <: 𝚫(X)
    (typeVariable: TypeVariableName) // X
    (bound: NonvariableType) // 𝚫(X)
    (typeEnv: TypeParameter list) // 𝚫
    =
    match typeEnv |> List.tryFind (fun tv -> tv.Name = typeVariable) with
    | None -> Error $"Type variable '{typeVariable |> typeVariableNameString}' not defined"
    | Some typeVariableDef ->
        if typeVariableDef.Bound = bound then
            Ok()
        else
            Error "S-Var does not hold"
