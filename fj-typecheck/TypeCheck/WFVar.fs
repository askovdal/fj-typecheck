module TypeCheck.WFVar

open AST
open Utils

let wfVar // Δ ⊢ X ok
    (typeVariableName: TypeVariableName) // X
    (typeEnv: TypeParameter list) // Δ
    =
    if
        typeEnv
        |> List.exists (fun typeVariable -> typeVariable.Name = typeVariableName)
    then
        Ok()
    else
        Error $"Type variable '{typeVariableName |> typeVariableNameString}' not defined"
