module TypeCheck.STrans

open AST
open ClassTable
open Utils

// Returns either the type variable's bound or the class' superclass.
let tryFindTypeBound (typeDef: Type) (typeEnv: TypeParameter list) (classTable: ClassTable) =
    match typeDef with
    | TypeVariable typeVariableName ->
        match typeEnv |> List.tryFind (fun tp -> tp.Name = typeVariableName) with
        | None -> Error $"Type variable '{typeVariableName |> typeVariableNameString}' not defined"
        | Some typeParameter -> Ok typeParameter.Bound

    | NonvariableType nonvariableType ->
        match classTable |> ClassTable.tryFind nonvariableType.ClassName with
        | None -> Error $"Class '{nonvariableType.ClassName |> classNameString}' not defined"
        | Some classDef -> Ok classDef.Superclass

let rec sTrans // 𝚫 ⊢ S <: U
    (subType: Type) // S
    (superType: Type) // U
    (typeEnv: TypeParameter list) // 𝚫
    (classTable: ClassTable)
    =
    tryFindTypeBound subType typeEnv classTable
    |> Result.bind (fun bound -> // T
        match classTable |> ClassTable.tryFind bound.ClassName with
        | None -> Error $"Class '{bound.ClassName |> classNameString}' not defined"
        | Some boundClass ->
            boundClass.Superclass
            |> substituteInNvType bound.TypeArguments boundClass.TypeParameters
            |> Result.bind (fun substitutedSuperclass ->
                // Check if 𝚫 ⊢ T <: U holds using S-Class
                if NonvariableType substitutedSuperclass = superType then
                    Ok(Some())
                // If T's bound is Object, return Error, as we know U isn't Object as S-Class doesn't hold
                elif boundClass.Superclass.ClassName |> isObject then
                    Ok(None)
                else
                    // Check if 𝚫 ⊢ T <: U holds using S-Trans
                    sTrans (NonvariableType bound) superType typeEnv classTable))
