module Tests.PaperExample

open AST

(*
class A extends Object {
    A() {
        super();
    }
}

class B extends Object {
    B() {
        super();
    }
}

class Pair<X extends Object, Y extends Object> extends Object {
    X fst;
    Y snd;

    Pair(X fst, Y snd) {
        super();
        this.fst = fst;
        this.snd = snd;
    }

    <Z extends Object> Pair<Z, Y> setfst(Z newfst) {
        return new Pair<Z, Y>(newfst, this.snd);
    }
}
*)

let objectType =
    { ClassName = ClassName "Object"
      TypeArguments = [] }

let classAConstructor = { Parameters = [] }

let classA =
    { Name = ClassName "A"
      TypeParameters = []
      Superclass = objectType
      Fields = []
      Constructor = classAConstructor
      Methods = [] }

let classBConstructor = { Parameters = [] }

let classB =
    { Name = ClassName "B"
      TypeParameters = []
      Superclass = objectType
      Fields = []
      Constructor = classBConstructor
      Methods = [] }

let classPairConstructor =
    { Parameters =
        [ { Type = TypeVariable(TypeVariableName "X")
            Name = ParameterName "fst" }
          { Type = TypeVariable(TypeVariableName "Y")
            Name = ParameterName "snd" } ] }

let pairZY =
    { ClassName = ClassName "Pair"
      TypeArguments = [ TypeVariable(TypeVariableName "Z"); TypeVariable(TypeVariableName "Y") ] }


let setFst =
    { TypeParameters =
        [ { Name = TypeVariableName "Z"
            Bound = objectType
            Variance = Invariant } ]
      ReturnType = NonvariableType pairZY
      Name = MethodName "setfst"
      Parameters =
        [ { Type = TypeVariable(TypeVariableName "Z")
            Name = ParameterName "newfst" } ]
      Return =
        NewInstance
            { Class = pairZY
              Parameters =
                [ Variable "newfst"
                  FieldAccess
                      { Object = Variable "this"
                        Field = FieldName "snd" } ] } }

let classPair =
    { Name = ClassName "Pair"
      TypeParameters =
        [ { Name = TypeVariableName "X"
            Bound = objectType
            Variance = Invariant }
          { Name = TypeVariableName "Y"
            Bound = objectType
            Variance = Invariant } ]
      Superclass = objectType
      Fields =
        [ { Type = TypeVariable(TypeVariableName "X")
            Name = FieldName "fst" }
          { Type = TypeVariable(TypeVariableName "Y")
            Name = FieldName "snd" } ]
      Constructor = classPairConstructor
      Methods = [ setFst ] }
