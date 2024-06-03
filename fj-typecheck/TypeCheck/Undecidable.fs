module TypeCheck.Undecidable
open AST
open ClassTable
open Utils

let rec height =
    function
    | TypeVariable _ -> 1
    | NonvariableType { TypeArguments = typeArguments } when typeArguments |> List.isEmpty -> 1
    | NonvariableType { TypeArguments = typeArguments } -> 1 + (typeArguments |> List.map height |> List.max)

let declaredSuper (typeDef: Type) ((typeEnv, classTable, _): State) =
    match typeDef with
    | TypeVariable typeVariableName ->
        match typeEnv |> List.tryFind (fun tp -> tp.Name = typeVariableName) with
        | None -> Error $"Type variable '{typeVariableName |> typeVariableNameString}' not defined"
        | Some { Bound = bound } -> Ok bound // 𝚫(T)
    | NonvariableType nonvariableType -> // C<T̄>
        classTable
        |> ClassTable.find nonvariableType.ClassName
        |> Result.bind (fun subtypeClassDef -> // C<X̄>
            subtypeClassDef.Superclass // V
            |> substituteInNvType nonvariableType.TypeArguments subtypeClassDef.TypeParameters) // [T̄/X̄]V

let rec transSuper (subtype: Type) (superType: Type) (state: State) =
    match superType with
    | TypeVariable _ -> Ok false
    | NonvariableType superNvType ->
        match subtype with
        | NonvariableType subNvType when subNvType |> isObject -> Ok false
        | _ ->
            declaredSuper subtype state
            |> Result.bind (fun v ->
                if v = superNvType then
                    Ok true
                else
                    transSuper (NonvariableType v) superType state)

let undecidable ((t1, u1): SubtypeAssertion) ((t2, u2): SubtypeAssertion) (state: State) =
    let _, _, expansive = state

    transSuper t2 t1 state
    |> Result.map (fun transSuperResult ->
        if height u1 < height u2 || (height u1 = height u2 && transSuperResult) then
            false
        else
            expansive)
