module Program

open Examples.ContrivedExample
open TypeCheck.ClassTable
open ClassTable

[<EntryPoint>]
let main _argv =
    let classTable = ClassTable.empty |> ClassTable.addClasses [ classA; classB; classC; classD ]

    match classTable |> typeCheckClassTable with
    | Ok _ ->
        printfn "Type check success"
        0
    | Error err ->
        printfn $"%A{err}"
        1
