module TypeCheck.STrans

open AST
open ClassTable
open Utils
open TypeCheck.SClass

// Returns either the type variable's bound or the class' superclass.
let tryFindTypeBound (typeDef: Type) (typeEnv: TypeParameter list) (classTable: ClassTable) =
    match typeDef with
    | TypeVariable typeVariableName ->
        match typeEnv |> List.tryFind (fun tp -> tp.Name = typeVariableName) with
        | None -> Error $"Type variable '{typeVariableName |> typeVariableNameString}' not defined"
        | Some typeParameter -> Ok typeParameter.Bound

    | NonvariableType nonvariableType ->
        classTable
        |> ClassTable.find nonvariableType.ClassName
        |> Result.map (_.Superclass)

let rec sTrans // 𝚫 ⊢ S <: U
    (subType: Type) // S
    (superType: NonvariableType) // U (nvType because bounds cannot be type variables)
    (typeEnv: TypeParameter list) // 𝚫
    (classTable: ClassTable)
    =
    tryFindTypeBound subType typeEnv classTable
    |> Result.bind (fun bound -> // T
        // Check if 𝚫 ⊢ T <: U holds using S-Class
        match sClass bound superType classTable with
        | Ok() -> Ok(Some())
        | Error _ ->
            classTable
            |> ClassTable.find bound.ClassName
            |> Result.bind (fun boundClassDef ->
                // If T's bound is Object, return None, as we know U isn't Object as S-Class doesn't hold
                if boundClassDef.Superclass.ClassName |> isObject then
                    Ok(None)
                else
                    // Check if 𝚫 ⊢ T <: U holds using S-Trans
                    sTrans (NonvariableType bound) superType typeEnv classTable))
