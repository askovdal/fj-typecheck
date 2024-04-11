module TypeCheck.TypeParameters

open AST
open TypeCheck.ClassFields
open ClassTable
open Utils

type TypeParameterState = TypeParameter * Class * ClassTable

let boundHasRightAmountOfTypeArguments (typeArguments: Type list) (boundClass: Class) =
    let typeArgumentsLength = typeArguments |> List.length
    let typeParametersLength = boundClass.TypeParameters |> List.length

    if typeArgumentsLength <> typeParametersLength then
        Error $"Bound expects {typeParametersLength} type arguments, got {typeArgumentsLength}"
    else
        Ok()

type TypeArgumentState = Type * TypeParameter * Class * ClassTable

let rec classExtendsCorrectClass (classExtends: ClassName) (shouldExtend: ClassName) (classTable: ClassTable) =
    match classExtends with
    | className when className = shouldExtend -> Ok()
    | className when className |> isObject ->
        Error $"Type argument does not extend correct class, should extend '{shouldExtend |> classNameString}'"
    | className ->
        match classTable |> ClassTable.tryFind className with
        | None -> Error $"Class '{className |> classNameString}' not defined"
        | Some boundClass -> classExtendsCorrectClass boundClass.Superclass.ClassName shouldExtend classTable


let rec typeCheckNvTypeArguments
    (boundClass: Class)
    (typeArguments: Type list)
    (classDef: Class)
    (classTable: ClassTable)
    =
    let folder (state: Result<unit, string>) (typeArgument: Type) (typeParameter: TypeParameter) =
        state
        |> Result.bind (fun _ -> typeCheckTypeArgument typeArgument typeParameter classDef classTable)

    boundHasRightAmountOfTypeArguments typeArguments boundClass
    |> Result.bind (fun _ -> (Ok(), typeArguments, boundClass.TypeParameters) |||> List.fold2 folder)

/// Type checks a type argument against its corresponding type parameter.
and typeCheckTypeArgument
    (typeArgument: Type)
    (typeParameter: TypeParameter)
    (classDef: Class)
    (classTable: ClassTable)
    =
    let shouldExtend = typeParameter.Bound.ClassName

    let result =
        match typeArgument with
        | TypeVariable typeArgumentTypeVariable ->
            typeVariableDefined classDef typeArgumentTypeVariable
            |> Result.bind (fun typeVariable ->
                classExtendsCorrectClass typeVariable.Bound.ClassName shouldExtend classTable)

        | NonvariableType typeArgumentNvType ->
            match classTable |> ClassTable.tryFind typeArgumentNvType.ClassName with
            | None -> Error $"Class '{typeArgumentNvType.ClassName |> classNameString}' not defined"
            | Some typeArgumentClass ->
                let classExtends = typeArgumentClass.Superclass.ClassName

                classExtendsCorrectClass classExtends shouldExtend classTable
                |> Result.bind (fun _ ->
                    let typeArguments = typeArgumentNvType.TypeArguments
                    typeCheckNvTypeArguments typeArgumentClass typeArguments classDef classTable)

    match result with
    | Ok _ -> Ok()
    | Error error -> Error $"Error in type argument '{typeArgument |> debugType}': {error}"

let boundOk (state: TypeParameterState) =
    let typeParameter, classDef, classTable = state
    let typeArguments = typeParameter.Bound.TypeArguments

    let result =
        match classTable |> ClassTable.tryFind typeParameter.Bound.ClassName with
        | None -> Error "Bound is undefined"
        | Some boundClass -> typeCheckNvTypeArguments boundClass typeArguments classDef classTable

    match result with
    | Ok _ -> Ok()
    | Error error -> Error $"Error in bound '{typeParameter.Bound.ClassName |> classNameString}': {error}"

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

let typeCheckTypeParameter typeParameter ((classDef, classTable): State) () =
    let result =
        Ok(typeParameter, classDef, classTable)
        |> Result.bind typeParameterNameIsUnique
        |> Result.bind boundOk

    match result with
    | Ok _ -> Ok()
    | Error error -> Error $"Error in type parameter '{typeParameter.Name |> typeVariableNameString}': {error}"


let typeCheckTypeParameters ((classDef, classTable): State) =
    let folder (state: Result<unit, string>) (typeParameter: TypeParameter) =
        state
        |> Result.bind (typeCheckTypeParameter typeParameter (classDef, classTable))

    (Ok(), classDef.TypeParameters) ||> List.fold folder
