module TypeCheck.WFVar

open AST
open Utils

let wfVar // 𝚫 ⊢ X ok
    (typeVariableName: TypeVariableName) // X
    (typeEnv: TypeParameter list) // 𝚫
    =
    if
        typeEnv
        |> List.exists (fun typeVariable -> typeVariable.Name = typeVariableName)
    then
        Ok()
    else
        Error $"Type variable '{typeVariableName |> typeVariableNameString}' not defined"
