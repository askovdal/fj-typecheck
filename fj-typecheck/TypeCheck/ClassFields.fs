module TypeCheck.ClassFields

open AST

type FieldState = Field * Class * ClassTable

let typeVariableDefined classDef typeVariableName =
    let typeParameterNameMatch ({ Name = name }: TypeParameter) = name = typeVariableName

    match classDef.TypeParameters |> List.tryFind typeParameterNameMatch with
    | Some typeVariable -> Ok(typeVariable)
    | None -> Error $"Type variable '{typeVariableName |> typeVariableNameString}' not defined in type parameters"

let typeCheckFieldType ((field, classDef, classTable): FieldState) =
    match field.Type with
    | TypeVariable typeVariableName -> typeVariableDefined classDef typeVariableName |> Result.map (fun _ -> ())
    | NonvariableType _ ->
        // TODO: Implement
        Ok()

let fieldNameDistinctFromSuperclass ((field, classDef, classTable): FieldState) =
    match classTable |> ClassTable.tryFind classDef.Superclass.ClassName with
    | None -> Error $"Superclass '{classDef.Superclass.ClassName |> classNameString}' undefined"
    | Some superclass ->
        let fieldNameMatch ({ Name = name }: Field) = name = field.Name

        if superclass.Fields |> List.exists fieldNameMatch then
            Error $"Field already defined in superclass '{superclass.Name |> classNameString}'"
        else
            Ok(field, classDef, classTable)

let fieldNameIsUnique ((field, classDef, classTable): FieldState) =
    let fieldNameMatch (otherField: Field) = otherField.Name = field.Name

    match classDef.Fields |> List.filter fieldNameMatch |> List.tryExactlyOne with
    | None -> Error "Field name is defined more than once"
    | Some _ -> Ok(field, classDef, classTable)

let typeCheckClassField ((field, classDef, classTable): FieldState) =
    let result =
        Ok(field, classDef, classTable)
        |> Result.bind fieldNameIsUnique
        |> Result.bind fieldNameDistinctFromSuperclass
        |> Result.bind typeCheckFieldType

    match result with
    | Ok _ -> Ok(classDef, classTable)
    | Error err -> Error $"Error in field '{field.Name |> fieldNameString}': {err}"

let typeCheckClassFields ((classDef, classTable): State) =
    let folder (state: Result<State, string>) (field: Field) =
        state
        |> Result.bind (fun _ -> typeCheckClassField (field, classDef, classTable))

    classDef.Fields |> List.fold folder (Ok(classDef, classTable))
