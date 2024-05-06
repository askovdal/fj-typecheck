module TypeCheck.SubtypeAssertion

open AST
open ClassTable
open Utils
open TypeCheck.SRefl

let rec subtypeAssertion // 𝚫 ⊢ T <: U
    (subtype: Type) // T
    (supertype: Type) // U
    (typeEnv: TypeParameter list) // 𝚫
    (classTable: ClassTable)
    =
    printfn $"{subtype |> debugType} <: {supertype |> debugType}"

    // First, check if S-Refl holds
    sRefl subtype supertype
    |> orElse (fun _ ->
        match supertype with
        | TypeVariable _ -> Error "Bounds cannot be type variables"
        | NonvariableType superNvType -> // U = D<Ū>
            match subtype with
            // If T = Object, return Error, as we now U isn't Object as S-Refl doesn't hold
            | NonvariableType subNvType when subNvType |> isObject -> Error "'Object' does not have a supertype"

            // If T = C<T̄> and C = D, check if Var rule holds
            | NonvariableType subNvType when subNvType.ClassName = superNvType.ClassName ->
                variance subNvType superNvType typeEnv classTable

            | NonvariableType subNvType -> super subNvType superNvType typeEnv classTable

            | TypeVariable subtypeVar -> bound subtypeVar superNvType typeEnv classTable)

and variance // 𝚫 ⊢ C<T̄> <: C<Ū>
    (subtype: NonvariableType) // C<T̄>
    (supertype: NonvariableType) // C<Ū>
    (typeEnv: TypeParameter list) // 𝚫
    (classTable: ClassTable)
    =
    let varianceHolds // 𝚫 ⊢ Ti <:var(C#i) Ui
        (subtypeArgument: Type) // Ti
        (supertypeArgument: Type) // Ui
        (var: Variance) // var(C#i)
        ()
        =
        match var with
        | Invariant ->
            sRefl subtypeArgument supertypeArgument // 𝚫 ⊢ Ti = Ui
            |> prefixError $"Error in '{subtypeArgument |> debugType}' <:₀ '{supertypeArgument |> debugType}':"

        | Covariant ->
            subtypeAssertion subtypeArgument supertypeArgument typeEnv classTable // 𝚫 ⊢ Ti <: Ui
            |> prefixError $"Error in '{subtypeArgument |> debugType}' <:₊ '{supertypeArgument |> debugType}':"

        | Contravariant ->
            subtypeAssertion supertypeArgument subtypeArgument typeEnv classTable // 𝚫 ⊢ Ui <: Ti
            |> prefixError $"Error in '{subtypeArgument |> debugType}' <:₋ '{supertypeArgument |> debugType}':"

    let folder
        (state: Result<unit, string>)
        (subtypeArgument: Type, // Ti
         supertypeArgument: Type, // Ui
         typeParam: TypeParameter) // C#i
        =
        state
        |> Result.bind (varianceHolds subtypeArgument supertypeArgument typeParam.Variance)

    classTable
    |> ClassTable.find subtype.ClassName
    |> Result.bind (fun classDef ->
        (Ok(), List.zip3 subtype.TypeArguments supertype.TypeArguments classDef.TypeParameters)
        ||> List.fold folder)

and super // 𝚫 ⊢ C<T̄> <: D<Ū>
    (subtype: NonvariableType) // C<T̄>
    (supertype: NonvariableType) // D<Ū>
    (typeEnv: TypeParameter list) // 𝚫
    (classTable: ClassTable)
    =
    classTable
    |> ClassTable.find subtype.ClassName
    |> Result.bind (fun subtypeClassDef -> // C<X̄>
        subtypeClassDef.Superclass // V
        |> substituteInNvType subtype.TypeArguments subtypeClassDef.TypeParameters)
    |> Result.bind (fun substitutedSuperClass -> // [T̄/X̄]V
        // [T̄/X̄]V <: D<Ū>
        subtypeAssertion (NonvariableType substitutedSuperClass) (NonvariableType supertype) typeEnv classTable)

and bound // 𝚫 ⊢ X <: D<Ū>
    (subtype: TypeVariableName) // X
    (supertype: NonvariableType) // D<Ū>
    (typeEnv: TypeParameter list) // 𝚫
    (classTable: ClassTable)
    =
    match typeEnv |> List.tryFind (fun tp -> tp.Name = subtype) with
    | None -> Error $"Type variable '{subtype |> typeVariableNameString}' not defined"
    | Some { Bound = bound } -> // 𝚫(X)
        // 𝚫(X) <: D<Ū>
        subtypeAssertion (NonvariableType bound) (NonvariableType supertype) typeEnv classTable
