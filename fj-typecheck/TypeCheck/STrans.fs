module TypeCheck.STrans

open AST
open ClassTable
open Utils
open TypeCheck.SRefl

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
        |> Result.bind (fun nvTypeClassDef ->
            nvTypeClassDef.Superclass
            |> substituteInNvType nonvariableType.TypeArguments nvTypeClassDef.TypeParameters)

let rec sTrans // 𝚫 ⊢ S <: U
    (subType: Type) // S
    (superType: NonvariableType) // U (nvType because bounds cannot be type variables)
    (typeEnv: TypeParameter list) // 𝚫
    (classTable: ClassTable)
    =
    tryFindTypeBound subType typeEnv classTable
    |> Result.bind (fun bound -> // T
        // 𝚫 ⊢ T <: U
        checkSubTypeRelation (NonvariableType bound) (NonvariableType superType) typeEnv classTable)

and variance // 𝚫 ⊢ C<T̄> <: C<Ū>
    (subType: NonvariableType) // C<T̄>
    (superType: NonvariableType) // C<Ū>
    (typeEnv: TypeParameter list) // 𝚫
    (classTable: ClassTable)
    =
    let varianceHolds // 𝚫 ⊢ Ti <:var(C#i) Ui
        (subTypeArgument: Type) // Ti
        (superTypeArgument: Type) // Ui
        (var: Variance) // var(C#i)
        ()
        =
        match var with
        | Invariant ->
            sRefl subTypeArgument superTypeArgument // 𝚫 ⊢ Ti = Ui
            |> prefixError $"Error in '{subTypeArgument |> debugType}' <:₀ '{superTypeArgument |> debugType}':"

        | Covariant ->
            checkSubTypeRelation subTypeArgument superTypeArgument typeEnv classTable // 𝚫 ⊢ Ti <: Ui
            |> prefixError $"Error in '{subTypeArgument |> debugType}' <:₊ '{superTypeArgument |> debugType}':"

        | Contravariant ->
            checkSubTypeRelation superTypeArgument subTypeArgument typeEnv classTable // 𝚫 ⊢ Ui <: Ti
            |> prefixError $"Error in '{subTypeArgument |> debugType}' <:₋ '{superTypeArgument |> debugType}':"

    let folder
        (state: Result<unit, string>)
        (subTypeArgument: Type, // Ti
         superTypeArgument: Type, // Ui
         typeParam: TypeParameter) // C#i
        =
        state
        |> Result.bind (varianceHolds subTypeArgument superTypeArgument typeParam.Variance)

    classTable
    |> ClassTable.find subType.ClassName
    |> Result.bind (fun classDef ->
        (Ok(), List.zip3 subType.TypeArguments superType.TypeArguments classDef.TypeParameters)
        ||> List.fold folder)

and checkSubTypeRelation // 𝚫 ⊢ T <: U
    (subType: Type) // T
    (superType: Type) // U
    (typeEnv: TypeParameter list) // 𝚫
    (classTable: ClassTable)
    =
    // First, check if S-Refl holds
    sRefl subType superType
    |> orElse (fun _ ->
        match superType with
        | TypeVariable _ -> Error "Bounds cannot be type variables"
        | NonvariableType superNvType -> // U = D<Ū>
            match subType with
            // If T = Object, return Error, as we now U isn't Object as S-Refl doesn't hold
            | NonvariableType subNvType when subNvType |> isObject -> Error "'Object' does not have a supertype"

            // If T = C<T̄> and C = D, check if Variance rule holds
            | NonvariableType subNvType when subNvType.ClassName = superNvType.ClassName ->
                variance subNvType superNvType typeEnv classTable

            // C ≠ D, so check if S-Trans holds
            | _ -> sTrans subType superNvType typeEnv classTable)
