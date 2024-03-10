module TypeCheck.ClassGenerics

open AST

let boundsAreDefined ((classDef, classTable): State) =
    let boundIsUndefined { Bound = bound } =
        classTable |> ClassTable.containsClass bound.ClassName |> not

    match classDef.Generics |> List.tryFind boundIsUndefined with
    | None -> Ok(classDef, classTable)
    | Some { Bound = bound; Name = name } ->
        Error
            $"Bound '{bound.ClassName |> classNameString}' of type variable '{name |> typeVariableNameString}' is undefined"

let classGenericsAreUnique ((classDef, classTable): State) =
    let genericNameIsDuplicate (g1: TypeParameter) =
        classDef.Generics
        |> List.filter (fun g2 -> g2.Name = g1.Name)
        |> List.length
        |> (<) 1

    match classDef.Generics |> List.tryFind genericNameIsDuplicate with
    | None -> Ok(classDef, classTable)
    | Some generic -> Error $"Type variable '{generic.Name |> typeVariableNameString}' is defined more than once"

let typeCheckClassGenerics ((classDef, classTable): State) =
    Ok(classDef, classTable)
    |> Result.bind classGenericsAreUnique
    |> Result.bind boundsAreDefined
