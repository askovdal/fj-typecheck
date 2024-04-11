module TypeCheck.ClassTable

open AST
open GTClass
open ClassTable

let typeCheckClassTable (classTable: ClassTable) =
    let typeCheckClass (state: Result<unit, string>) (classDef: Class) =
        state |> Result.bind (gtClass classDef classTable)

    let classDefs = classTable |> Map.values |> Seq.toList

    (Ok(), classDefs) ||> List.fold typeCheckClass
