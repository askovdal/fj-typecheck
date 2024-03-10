module TypeCheck.Superclass

open AST

let superclassIsDefined ((classDef, classTable): State) =
    match classTable |> ClassTable.tryFind classDef.Superclass.ClassName with
    | None -> Error $"Superclass '{classDef.Superclass.ClassName |> classNameString}' undefined"
    | Some _ -> Ok(classDef, classTable)

let typeCheckSuperclass ((classDef, classTable): State) =
    superclassIsDefined (classDef, classTable)
