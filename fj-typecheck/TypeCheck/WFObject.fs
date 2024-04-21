module TypeCheck.WFObject

open AST
open Utils

let wfObject // 𝚫 ⊢ Object ok
    (nvType: NonvariableType)
    =
    if nvType |> isObject then
        Ok()
    else
        Error $"'{nvType |> debugNvType}' is not 'Object'"
