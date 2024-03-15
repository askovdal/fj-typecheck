module Program

open AST
open Tests.Other
open TypeCheck.Main

[<EntryPoint>]
let main _argv =
    let classTable = ClassTable.empty |> ClassTable.addClasses [ classA; classB ]

    match classTable |> typeCheckClassTable with
    | Ok _ ->
        printfn "Type check success"
        0
    | Error err ->
        printfn $"%A{err}"
        1
