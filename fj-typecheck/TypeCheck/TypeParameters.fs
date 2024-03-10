module TypeCheck.TypeParameters

open AST
open TypeCheck.ClassFields

type TypeParameterState = TypeParameter * Class * ClassTable

let boundHasRightAmountOfTypeArguments typeArguments boundClass =
    let typeArgumentsLength = typeArguments |> List.length
    let typeParametersLength = boundClass.TypeParameters |> List.length

    if typeArgumentsLength <> typeParametersLength then
        Error
            $"Bound '{boundClass.Name |> classNameString}' expects {typeParametersLength} type arguments, got {typeArgumentsLength}"
    else
        Ok()

type TypeArgumentState = Type * TypeParameter * Class * ClassTable

let typeVariableExtendsCorrectClass (state: TypeArgumentState) (typeVariable: TypeParameter) =
    let typeArgument, typeParameter, classDef, classTable = state
    let shouldExtend = typeParameter.Bound.ClassName

    match typeVariable.Bound.ClassName with
    | className when className = shouldExtend -> Ok()
    | className ->
        // TODO: Get className's superclass. If Object, the bound is not fulfilled. Else, recurse with superclass
        Error "implement"

let typeCheckTypeArgument (state: TypeArgumentState) =
    let typeArgument, typeParameter, classDef, classTable = state

    let result =
        match typeArgument with
        | TypeVariable typeVariableName ->
            typeVariableDefined classDef typeVariableName
            |> Result.bind (typeVariableExtendsCorrectClass state)
        | NonvariableType nonvariableType -> Ok()

    match result with
    | Ok _ -> Ok(typeParameter, classDef, classTable)
    | Error error -> Error $"Error in type argument '{typeParameter.Name |> typeVariableNameString}': {error}"

let typeCheckBoundsTypeArguments boundClass (state: TypeParameterState) =
    let typeParameter, classDef, classTable = state
    let typeArguments = typeParameter.Bound.TypeArguments

    let folder (state: Result<TypeParameterState, string>) (typeArgument: Type) (typeParameter: TypeParameter) =
        state
        |> Result.bind (fun _ -> typeCheckTypeArgument (typeArgument, typeParameter, classDef, classTable))

    boundHasRightAmountOfTypeArguments typeArguments boundClass
    |> Result.bind (fun _ -> (Ok(state), typeArguments, boundClass.TypeParameters) |||> List.fold2 folder)

let typeCheckBound (state: TypeParameterState) =
    let typeParameter, classDef, classTable = state

    match classTable |> ClassTable.tryFind typeParameter.Bound.ClassName with
    | None -> Error $"Bound '{typeParameter.Bound.ClassName |> classNameString}' is undefined"
    | Some boundClass -> typeCheckBoundsTypeArguments boundClass state

// TODO: Check that each nonvariable type argument is defined
// TODO: Check that each nonvariable type argument either is or extends the correct class

let typeParameterNameIsUnique ((typeParameter, classDef, classTable): TypeParameterState) =
    let typeParameterNameMatch (otherTypeParameter: TypeParameter) =
        otherTypeParameter.Name = typeParameter.Name

    match
        classDef.TypeParameters
        |> List.filter typeParameterNameMatch
        |> List.tryExactlyOne
    with
    | None -> Error "Type parameter is defined more than once"
    | Some _ -> Ok(typeParameter, classDef, classTable)

let typeCheckTypeParameter ((typeParameter, classDef, classTable): TypeParameterState) =
    let result =
        Ok(typeParameter, classDef, classTable)
        |> Result.bind typeParameterNameIsUnique
        |> Result.bind typeCheckBound

    match result with
    | Ok _ -> Ok(classDef, classTable)
    | Error error -> Error $"Error in type parameter '{typeParameter.Name |> typeVariableNameString}': {error}"


let typeCheckTypeParameters ((classDef, classTable): State) =
    let folder (state: Result<State, string>) (typeParameter: TypeParameter) =
        state
        |> Result.bind (fun (classDef, classTable) -> typeCheckTypeParameter (typeParameter, classDef, classTable))

    classDef.TypeParameters |> List.fold folder (Ok(classDef, classTable))
