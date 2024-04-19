module Examples.UndecidabilityExamples

open AST

(*
class N<-Z extends Object> extends Object {}

class C extends N<N<C> {}

class Tester<X extends N<C>, Y extends Tester<C, Y>> extends Object {}

----

class N<-Z extends Object> extends Object {}

class C<X extends Object> extends N<N<C<C<X>>>> {}

class T extends Object {}
class U extends Object {}

class Tester<X extends N<C<U>>, Y extends Tester<C<T>, Y>> extends Object {}
*)

module Example1 =
    let classN =
        { Name = ClassName "N"
          TypeParameters =
            [ { Variance = Contravariant
                Name = TypeVariableName "Z"
                Bound =
                  { ClassName = ClassName "Object"
                    TypeArguments = [] } } ]
          Superclass =
            { ClassName = ClassName "Object"
              TypeArguments = [] }
          Fields = []
          Constructor = { Parameters = [] }
          Methods = [] }

    let classC =
        { Name = ClassName "C"
          TypeParameters = []
          Superclass =
            { ClassName = ClassName "N"
              TypeArguments =
                [ NonvariableType
                      { ClassName = ClassName "N"
                        TypeArguments =
                          [ NonvariableType
                                { ClassName = ClassName "C"
                                  TypeArguments = [] } ] } ] }
          Fields = []
          Constructor = { Parameters = [] }
          Methods = [] }

    let tester =
        { Name = ClassName "Tester"
          TypeParameters =
            [ { Variance = Invariant
                Name = TypeVariableName "X"
                Bound =
                  { ClassName = ClassName "N"
                    TypeArguments =
                      [ NonvariableType
                            { ClassName = ClassName "C"
                              TypeArguments = [] } ] } }
              { Variance = Invariant
                Name = TypeVariableName "Y"
                Bound =
                  { ClassName = ClassName "Tester"
                    TypeArguments =
                      [ NonvariableType
                            { ClassName = ClassName "C"
                              TypeArguments = [] }
                        TypeVariable(TypeVariableName "Y") ] } } ]
          Superclass =
            { ClassName = ClassName "Object"
              TypeArguments = [] }
          Fields = []
          Constructor = { Parameters = [] }
          Methods = [] }

    let classDefs = [ classN; classC; tester ]


module Example2 =
    let classN =
        { Name = ClassName "N"
          TypeParameters =
            [ { Variance = Contravariant
                Name = TypeVariableName "Z"
                Bound =
                  { ClassName = ClassName "Object"
                    TypeArguments = [] } } ]
          Superclass =
            { ClassName = ClassName "Object"
              TypeArguments = [] }
          Fields = []
          Constructor = { Parameters = [] }
          Methods = [] }

    let classC =
        { Name = ClassName "C"
          TypeParameters =
            [ { Variance = Invariant
                Name = TypeVariableName "X"
                Bound =
                  { ClassName = ClassName "Object"
                    TypeArguments = [] } } ]
          Superclass =
            { ClassName = ClassName "N"
              TypeArguments =
                [ NonvariableType
                      { ClassName = ClassName "N"
                        TypeArguments =
                          [ NonvariableType
                                { ClassName = ClassName "C"
                                  TypeArguments =
                                    [ NonvariableType
                                          { ClassName = ClassName "C"
                                            TypeArguments = [ TypeVariable(TypeVariableName "X") ] } ] } ] } ] }
          Fields = []
          Constructor = { Parameters = [] }
          Methods = [] }

    let classT =
        { Name = ClassName "T"
          TypeParameters = []
          Superclass =
            { ClassName = ClassName "Object"
              TypeArguments = [] }
          Fields = []
          Constructor = { Parameters = [] }
          Methods = [] }

    let classU =
        { Name = ClassName "U"
          TypeParameters = []
          Superclass =
            { ClassName = ClassName "Object"
              TypeArguments = [] }
          Fields = []
          Constructor = { Parameters = [] }
          Methods = [] }

    let tester =
        { Name = ClassName "Tester"
          TypeParameters =
            [ { Variance = Invariant
                Name = TypeVariableName "X"
                Bound =
                  { ClassName = ClassName "N"
                    TypeArguments =
                      [ NonvariableType
                            { ClassName = ClassName "C"
                              TypeArguments =
                                [ NonvariableType
                                      { ClassName = ClassName "U"
                                        TypeArguments = [] } ] } ] } }
              { Variance = Invariant
                Name = TypeVariableName "Y"
                Bound =
                  { ClassName = ClassName "Tester"
                    TypeArguments =
                      [ NonvariableType
                            { ClassName = ClassName "C"
                              TypeArguments =
                                [ NonvariableType
                                      { ClassName = ClassName "T"
                                        TypeArguments = [] } ] }
                        TypeVariable(TypeVariableName "Y") ] } } ]
          Superclass =
            { ClassName = ClassName "Object"
              TypeArguments = [] }
          Fields = []
          Constructor = { Parameters = [] }
          Methods = [] }

    let classDefs = [ classN; classC; classT; classU; tester ]
