module ClassTable

open AST
open Utils

type ClassTable = Map<ClassName, Class>

type State = Class * ClassTable

module ClassTable =
    let empty: ClassTable = Map.empty

    let addClasses (classDefs: Class list) (classTable: ClassTable) =
        let addClass (classTable: ClassTable) (classDef: Class) =
            classTable |> Map.add classDef.Name classDef

        (classTable, classDefs) ||> List.fold addClass

    let tryFind (className: ClassName) (classTable: ClassTable) =
        classTable |> Map.tryFind className

    let find (className: ClassName) (classTable: ClassTable) =
        classTable |> tryFind className |> okOr $"Class '{className |> classNameString}' not defined"
