module TypeCheck.SRefl

open AST

let sRefl // 𝚫 ⊢ T <: T
    (nvType: NonvariableType) // T
    (bound: NonvariableType)
    =
    if nvType = bound then
        Ok()
    else
        Error "S-Refl doesn't hold"
