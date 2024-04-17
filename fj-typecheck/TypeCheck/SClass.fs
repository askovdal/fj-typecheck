module TypeCheck.SClass

open AST
open ClassTable
open Utils

let sClass // 𝚫 ⊢ C<T̄> <: [T̄/X̄]N
    (nvType: NonvariableType) // C<T̄>
    (substitutedBound: NonvariableType) // [T̄/X̄]N
    (classTable: ClassTable)
    =
    classTable
    |> ClassTable.find nvType.ClassName
    |> Result.bind (fun nvTypeClassDef -> // class C<X̄ ◁ N̄> ◁ N {...}
        nvTypeClassDef.Superclass
        |> substituteInNvType nvType.TypeArguments nvTypeClassDef.TypeParameters
        |> Result.bind (fun substitutedSuperclass ->
            if substitutedSuperclass = substitutedBound then
                Ok()
            else
                Error "S-Class doesn't hold"))
