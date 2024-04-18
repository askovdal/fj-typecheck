module Program

open Examples.ContrivedExample
open Examples.VarianceExample
open TypeCheck.ClassTable
open ClassTable

[<EntryPoint>]
let main _argv =
    // let classTable = ClassTable.empty |> ClassTable.addClasses [ classA; classB; classC; classD ]
    let classTable =
        ClassTable.empty
        |> ClassTable.addClasses
            [ complexNumber
              realNumber
              rationalNumber
              integer
              naturalNumber
              container
              processor
              intProcessor
              storage ]

    match classTable |> typeCheckClassTable with
    | Ok _ ->
        printfn "Type check success"
        0
    | Error err ->
        printfn $"%A{err}"
        1
