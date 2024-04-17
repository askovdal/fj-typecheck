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

    let containsClass (className: ClassName) (classTable: ClassTable) =
        className |> isObject || classTable |> Map.containsKey className

    let tryFind (className: ClassName) (classTable: ClassTable) =
        if className |> isObject then
            let objectClass =
                { Name = ClassName "Object"
                  TypeParameters = []
                  Superclass =
                    { ClassName = ClassName "Object"
                      TypeArguments = [] }
                  Fields = []
                  Constructor = { Parameters = [] }
                  Methods = [] }

            Some objectClass
        else
            classTable |> Map.tryFind className

    let find (className: ClassName) (classTable: ClassTable) =
        classTable |> tryFind className |> optionOkOr $"Class '{className}' not defined"
