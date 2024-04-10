module Tests.ContrivedExample

open AST

(*
class A<X extends B<X, A<X, Y>>, Y extends Object> extends B<A<X, Y>, Y> {
    A() {
        super();
    }
}

class B<X extends B<X, Y>, Y extends Object> extends Object {
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
        [ { Name = TypeVariableName "X"
            Bound =
              { ClassName = ClassName "B"
                TypeArguments =
                  [ TypeVariable(TypeVariableName "X")
                    NonvariableType
                        { ClassName = ClassName "A"
                          TypeArguments = [ TypeVariable(TypeVariableName "X"); TypeVariable(TypeVariableName "Y") ] } ] }
            Variance = Invariant }
          { Name = TypeVariableName "Y"
            Bound =
              { ClassName = ClassName "Object"
                TypeArguments = [] }
            Variance = Invariant } ]
      Superclass =
        { ClassName = ClassName "B"
          TypeArguments =
            [ NonvariableType
                  { ClassName = ClassName "A"
                    TypeArguments = [ TypeVariable(TypeVariableName "X"); TypeVariable(TypeVariableName "Y") ] }
              TypeVariable(TypeVariableName "Y") ] }
      Fields = []
      Constructor = { Parameters = [] }
      Methods = [] }

let classB =
    { Name = ClassName "B"
      TypeParameters =
        [ { Name = TypeVariableName "X"
            Bound =
              { ClassName = ClassName "B"
                TypeArguments = [ TypeVariable(TypeVariableName "X"); TypeVariable(TypeVariableName "Y") ] }
            Variance = Invariant }
          { Name = TypeVariableName "Y"
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
