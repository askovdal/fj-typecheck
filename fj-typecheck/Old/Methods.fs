module TypeCheck.Methods

open AST
open ClassTable

type MethodState = Method * Class * ClassTable

// Generics:
//  Names are unique
//  Bounds are defined


let typeCheckMethod ((method, classDef, classTable): MethodState) =
    
    
    Ok(classDef, classTable)

let typeCheckMethods ((classDef, classTable): State) =
    let folder (state: Result<State, string>) (method: Method) =
        state
        |> Result.bind (fun (classDef, classTable) -> typeCheckMethod (method, classDef, classTable))

    classDef.Methods |> List.fold folder (Ok(classDef, classTable))
