module TypeCheck.STrans

open AST
open ClassTable
open Utils
open TypeCheck.SClass
open TypeCheck.SVar
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
        // If T is C<T̄> and U is C<Ū>, check if Variance rule holds
        if bound.ClassName = superType.ClassName then
            variance bound superType typeEnv classTable
        else
            // Check if 𝚫 ⊢ T <: U holds using S-Class
            sClass bound superType classTable
            |> orElse (fun _ ->
                classTable
                |> ClassTable.find bound.ClassName
                |> Result.bind (fun boundClassDef ->
                    // If T's bound is Object, return Error, as we know U isn't Object as S-Class does not hold
                    if boundClassDef.Superclass.ClassName |> isObject then
                        Error "S-Trans does not hold"
                    else
                        // Check if 𝚫 ⊢ T <: U holds using S-Trans
                        sTrans (NonvariableType bound) superType typeEnv classTable)))

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
        | NonvariableType superNvType ->
            match subType with
            // If T is X, check if S-Var holds
            | TypeVariable subTypeVariable -> sVar subTypeVariable superNvType typeEnv

            // If T is C<T̄>...
            | NonvariableType subNvType ->
                // ...and U is C<Ū>...
                if subNvType.ClassName = superNvType.ClassName then
                    // ...check if Variance rule holds.
                    variance subNvType superNvType typeEnv classTable
                else
                    // Else, check if S-Class holds
                    sClass subNvType superNvType classTable

            // Lastly, check if S-Trans holds
            |> orElse (fun _ -> sTrans subType superNvType typeEnv classTable))
