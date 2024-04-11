module Tests.ContrivedExample

open AST

(*
class A<T extends B<U, A<T, U>>, U extends Object> extends B<A<T, U>, U> {
    A() {
        super();
    }
}

class B<V extends B<V, W>, W extends Object> extends Object {
    B() {
        super();
    }
}

class C<X extends B<X, X>> extends B<X, X> {
    C() {
        super();
    }
}
*)

let classA =
    { Name = ClassName "A"
      TypeParameters =
        [ { Name = TypeVariableName "T"
            Bound =
              { ClassName = ClassName "B"
                TypeArguments =
                  [ TypeVariable(TypeVariableName "U")
                    NonvariableType
                        { ClassName = ClassName "A"
                          TypeArguments = [ TypeVariable(TypeVariableName "T"); TypeVariable(TypeVariableName "U") ] } ] }
            Variance = Invariant }
          { Name = TypeVariableName "U"
            Bound =
              { ClassName = ClassName "Object"
                TypeArguments = [] }
            Variance = Invariant } ]
      Superclass =
        { ClassName = ClassName "B"
          TypeArguments =
            [ NonvariableType
                  { ClassName = ClassName "A"
                    TypeArguments = [ TypeVariable(TypeVariableName "T"); TypeVariable(TypeVariableName "U") ] }
              TypeVariable(TypeVariableName "U") ] }
      Fields = []
      Constructor = { Parameters = [] }
      Methods = [] }

let classB =
    { Name = ClassName "B"
      TypeParameters =
        [ { Name = TypeVariableName "V"
            Bound =
              { ClassName = ClassName "B"
                TypeArguments = [ TypeVariable(TypeVariableName "V"); TypeVariable(TypeVariableName "W") ] }
            Variance = Invariant }
          { Name = TypeVariableName "W"
            Bound =
              { ClassName = ClassName "Object"
                TypeArguments = [] }
            Variance = Invariant } ]
      Superclass =
        { ClassName = ClassName "Object"
          TypeArguments = [] }
      Fields = []
      Constructor = { Parameters = [] }
      Methods = [] }

let classC =
    { Name = ClassName "C"
      TypeParameters =
        [ { Name = TypeVariableName "X"
            Bound =
              { ClassName = ClassName "B"
                TypeArguments = [ TypeVariable(TypeVariableName "X"); TypeVariable(TypeVariableName "X") ] }
            Variance = Invariant } ]
      Superclass =
        { ClassName = ClassName "B"
          TypeArguments = [ TypeVariable(TypeVariableName "X"); TypeVariable(TypeVariableName "X") ] }
      Fields = []
      Constructor = { Parameters = [] }
      Methods = [] }
