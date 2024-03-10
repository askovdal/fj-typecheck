module TypeCheck.ClassFields

open AST

type FieldState = Field * Class * ClassTable

let typeCheckFieldType ((field, classDef, classTable): FieldState) =
    let typeVariableDefined typeVariableName =
        let typeParameterNameMatch ({ Name = name }: TypeParameter) = name = typeVariableName

        if classDef.Generics |> List.exists typeParameterNameMatch then
            Ok(field, classDef, classTable)
        else
            Error $"Type variable '{typeVariableName |> typeVariableNameString}' not defined in class generics"

    match field.Type with
    | TypeVariable typeVariableName -> typeVariableDefined typeVariableName
    | NonvariableType _ ->
        // TODO: Implement
        Ok(field, classDef, classTable)

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
    | None -> Error $"Field name '{field.Name |> fieldNameString}' is defined more than once"
    | Some _ -> Ok(field, classDef, classTable)

let typeCheckClassField ((field, classDef, classTable): FieldState) =
    let result =
        Ok(field, classDef, classTable)
        |> Result.bind fieldNameIsUnique
        |> Result.bind fieldNameDistinctFromSuperclass
        |> Result.bind typeCheckFieldType

    match result with
    | Ok(_, classDef, classTable) -> Ok(classDef, classTable)
    | Error err -> Error $"Error in field '{field.Name |> fieldNameString}': {err}"

let typeCheckClassFields ((classDef, classTable): State) =
    let folder (state: Result<Class * ClassTable, string>) (field: Field) =
        state
        |> Result.bind (fun (classDef, classTable) -> typeCheckClassField (field, classDef, classTable))

    classDef.Fields |> List.fold folder (Ok(classDef, classTable))
