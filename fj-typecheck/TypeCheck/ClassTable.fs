module TypeCheck.ClassTable

open AST
open GTClass
open ClassTable
open Graph

let typeCheckClassTable (classTable: ClassTable) : Result<unit, string> =
    let classDefs = classTable |> Map.values |> Seq.toList
    let expansive = classTableExpansive classTable classDefs

    let typeCheckClass (state: Result<unit, string>) (classDef: Class) =
        state |> Result.bind (gtClass classDef classTable expansive)

    (Ok(), classDefs) ||> List.fold typeCheckClass
