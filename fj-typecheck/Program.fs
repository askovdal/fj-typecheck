module Program

open ClassTable
open TypeCheck.ClassTable
// open Examples.ContrivedExample
open Examples.VarianceExample
// open Examples.UndecidabilityExamples.Example2

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

    // let classTable = ClassTable.empty |> ClassTable.addClasses classDefs

    match classTable |> typeCheckClassTable with
    | Ok _ ->
        printfn "Type check success"
        0
    | Error err ->
        printfn $"%A{err}"
        1
