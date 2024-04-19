module TypeCheck.SRefl

open AST

let sRefl // 𝚫 ⊢ T <: T
    (typeDef: Type) // T
    (bound: Type)
    =
    if typeDef = bound then
        Ok()
    else
        Error "S-Refl does not hold"
