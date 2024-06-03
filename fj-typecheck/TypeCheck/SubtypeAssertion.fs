module TypeCheck.SubtypeAssertion

open AST
open ClassTable
open Utils
open TypeCheck.SRefl
open TypeCheck.WFObject
open TypeCheck.Undecidable

let rec subtypeAssertion // 𝚫 ⊢ T <: U
    (subtype: Type) // T
    (supertype: Type) // U
    (visited: Visited)
    (state: State)
    =
    printfn $"{subtype |> debugType} <: {supertype |> debugType}"

    if visited |> List.contains (subtype, supertype) then
        Error "Infinite cycle"
    else
        if visited |> List.isEmpty |> not then
            undecidable (subtype, supertype) visited.Head state
        else
            Ok false
        |> Result.bind (fun undecidableResult ->
            if undecidableResult then
                Error "Undecidable"
            else
                let newVisited = (subtype, supertype) :: visited

                match supertype with
                | TypeVariable _ -> Error "Bounds cannot be type variables"
                | NonvariableType superNvType -> // U = D<Ū>
                    match subtype with
                    | TypeVariable subtypeVar -> bound subtypeVar superNvType newVisited state

                    // If T = C<T̄> and C = D, check if Var rule holds
                    | NonvariableType subNvType when subNvType.ClassName = superNvType.ClassName ->
                        variance subNvType superNvType newVisited state

                    | NonvariableType subNvType when not (subNvType |> isObject) ->
                        super subNvType superNvType newVisited state

                    | _ -> Error "Subtype relation does not hold")


and variance // 𝚫 ⊢ C<T̄> <: C<Ū>
    (subtype: NonvariableType) // C<T̄>
    (supertype: NonvariableType) // C<Ū>
    (visited: Visited)
    (state: State)
    =
    let _, classTable, _ = state

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
            subtypeAssertion subtypeArgument supertypeArgument visited state // 𝚫 ⊢ Ti <: Ui
            |> prefixError $"Error in '{subtypeArgument |> debugType}' <:₊ '{supertypeArgument |> debugType}':"

        | Contravariant ->
            subtypeAssertion supertypeArgument subtypeArgument visited state // 𝚫 ⊢ Ui <: Ti
            |> prefixError $"Error in '{subtypeArgument |> debugType}' <:₋ '{supertypeArgument |> debugType}':"

    let folder
        (state: Result<unit, string>)
        (subtypeArgument: Type, // Ti
         supertypeArgument: Type, // Ui
         typeParam: TypeParameter) // C#i
        =
        state
        |> Result.bind (varianceHolds subtypeArgument supertypeArgument typeParam.Variance)


    // Check if C = Object first, as it doesn't exist in the class table
    wfObject subtype
    |> orElse (fun _ ->
        classTable
        |> ClassTable.find subtype.ClassName
        |> Result.bind (fun classDef ->
            (Ok(), List.zip3 subtype.TypeArguments supertype.TypeArguments classDef.TypeParameters)
            ||> List.fold folder))



and super // 𝚫 ⊢ C<T̄> <: D<Ū>
    (subtype: NonvariableType) // C<T̄>
    (supertype: NonvariableType) // D<Ū>
    (visited: Visited)
    (state: State)
    =
    let _, classTable, _ = state

    classTable
    |> ClassTable.find subtype.ClassName
    |> Result.bind (fun subtypeClassDef -> // C<X̄>
        subtypeClassDef.Superclass // V
        |> substituteInNvType subtype.TypeArguments subtypeClassDef.TypeParameters)
    |> Result.bind (fun substitutedSuperClass -> // [T̄/X̄]V
        // [T̄/X̄]V <: D<Ū>
        subtypeAssertion (NonvariableType substitutedSuperClass) (NonvariableType supertype) visited state)

and bound // 𝚫 ⊢ X <: D<Ū>
    (subtype: TypeVariableName) // X
    (supertype: NonvariableType) // D<Ū>
    (visited: Visited)
    (state: State)
    =
    let typeEnv, _, _ = state

    match typeEnv |> List.tryFind (fun tp -> tp.Name = subtype) with
    | None -> Error $"Type variable '{subtype |> typeVariableNameString}' not defined"
    | Some { Bound = bound } -> // 𝚫(X)
        // 𝚫(X) <: D<Ū>
        subtypeAssertion (NonvariableType bound) (NonvariableType supertype) visited state
