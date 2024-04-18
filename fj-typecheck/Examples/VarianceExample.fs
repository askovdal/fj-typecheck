module Examples.VarianceExample

open AST

(*
class ComplexNumber extends Object {}
class RealNumber extends ComplexNumber {}
class RationalNumber extends RealNumber {}
class Integer extends RationalNumber {}
class NaturalNumber extends Integer {}

class Container<+A extends ComplexNumber> extends Object {
    Container() {
        super();
    }
}

class Processor<B extends Container<C>, C extends RealNumber> extends Object {
    Processor() {
        super();
    }
}

class IntProcessor<G extends Container<Integer>> extends Processor<G, Integer> {
    IntProcessor() {
        super();
    }
}

class Storage<D extends IntProcessor<E>, E extends Container<NaturalNumber>> extends Object {
    Storage() {
        super();
    }
}
*)

let complexNumber =
    { Name = ClassName "ComplexNumber"
      TypeParameters = []
      Superclass =
        { ClassName = ClassName "Object"
          TypeArguments = [] }
      Fields = []
      Constructor = { Parameters = [] }
      Methods = [] }

let realNumber =
    { Name = ClassName "RealNumber"
      TypeParameters = []
      Superclass =
        { ClassName = ClassName "ComplexNumber"
          TypeArguments = [] }
      Fields = []
      Constructor = { Parameters = [] }
      Methods = [] }

let rationalNumber =
    { Name = ClassName "RationalNumber"
      TypeParameters = []
      Superclass =
        { ClassName = ClassName "RealNumber"
          TypeArguments = [] }
      Fields = []
      Constructor = { Parameters = [] }
      Methods = [] }

let integer =
    { Name = ClassName "Integer"
      TypeParameters = []
      Superclass =
        { ClassName = ClassName "RationalNumber"
          TypeArguments = [] }
      Fields = []
      Constructor = { Parameters = [] }
      Methods = [] }

let naturalNumber =
    { Name = ClassName "NaturalNumber"
      TypeParameters = []
      Superclass =
        { ClassName = ClassName "Integer"
          TypeArguments = [] }
      Fields = []
      Constructor = { Parameters = [] }
      Methods = [] }

let container =
    { Name = ClassName "Container"
      TypeParameters =
        [ { Name = TypeVariableName "A"
            Bound =
              { ClassName = ClassName "ComplexNumber"
                TypeArguments = [] }
            Variance = Covariant } ]
      Superclass =
        { ClassName = ClassName "Object"
          TypeArguments = [] }
      Fields = []
      Constructor = { Parameters = [] }
      Methods = [] }

let processor =
    { Name = ClassName "Processor"
      TypeParameters =
        [ { Name = TypeVariableName "B"
            Bound =
              { ClassName = ClassName "Container"
                TypeArguments = [ TypeVariable(TypeVariableName "C") ] }
            Variance = Invariant }
          { Name = TypeVariableName "C"
            Bound =
              { ClassName = ClassName "RealNumber"
                TypeArguments = [] }
            Variance = Invariant } ]
      Superclass =
        { ClassName = ClassName "Object"
          TypeArguments = [] }
      Fields = []
      Constructor = { Parameters = [] }
      Methods = [] }

let intProcessor =
    { Name = ClassName "IntProcessor"
      TypeParameters =
        [ { Name = TypeVariableName "G"
            Bound =
              { ClassName = ClassName "Container"
                TypeArguments =
                  [ NonvariableType
                        { ClassName = ClassName "Integer"
                          TypeArguments = [] } ] }
            Variance = Invariant } ]
      Superclass =
        { ClassName = ClassName "Processor"
          TypeArguments =
            [ TypeVariable(TypeVariableName "G")
              NonvariableType
                  { ClassName = ClassName "Integer"
                    TypeArguments = [] } ] }
      Fields = []
      Constructor = { Parameters = [] }
      Methods = [] }

let storage =
    { Name = ClassName "Storage"
      TypeParameters =
        [ { Name = TypeVariableName "D"
            Bound =
              { ClassName = ClassName "IntProcessor"
                TypeArguments = [ TypeVariable(TypeVariableName "E") ] }
            Variance = Invariant }
          { Name = TypeVariableName "E"
            Bound =
              { ClassName = ClassName "Container"
                TypeArguments =
                  [ NonvariableType
                        { ClassName = ClassName "NaturalNumber"
                          TypeArguments = [] } ] }
            Variance = Invariant } ]
      Superclass =
        { ClassName = ClassName "Object"
          TypeArguments = [] }
      Fields = []
      Constructor = { Parameters = [] }
      Methods = [] }
