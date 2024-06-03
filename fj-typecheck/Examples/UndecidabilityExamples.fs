module Examples.UndecidabilityExamples

open AST

(*
----------
Example 1:
----------
class N<-Z extends Object> extends Object {}

class C extends N<N<C> {}

class Tester<X extends N<C>, Y extends Tester<C, Y>> extends Object {}

----------
Example 1A (using bounds):
----------
class N<-Z extends Object> extends Object {}

class Tester<X extends N<N<X>>, Z extends N<X>, Y extends Tester<X, X, Y>> extends Object {}

----------
Example 2:
----------
class N<-Z extends Object> extends Object {}

class C<X extends Object> extends N<N<C<C<X>>>> {}

class T extends Object {}
class U extends Object {}

class Tester<X extends N<C<U>>, Y extends Tester<C<T>, Y>> extends Object {}

----------
Example 3 (exponential):
----------
class N<-Z extends Object> extends Object {}

class C0<X extends Object> extends N<N<X>>> {}
class C1<Y extends Object> extends C0<C0<Y>> {}
class C2<W extends Object> extends C1<C1<W>> {}

class T extends Object {}

class Tester<X extends N<C2<T>>, Y extends Tester<C2<N<T>>, Y>> extends Object {}
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

module Example3 =
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

    let classC0 =
        { Name = ClassName "C0"
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
                        TypeArguments = [ TypeVariable(TypeVariableName "X") ] } ] }
          Fields = []
          Constructor = { Parameters = [] }
          Methods = [] }

    let classC1 =
        { Name = ClassName "C1"
          TypeParameters =
            [ { Variance = Invariant
                Name = TypeVariableName "Y"
                Bound =
                  { ClassName = ClassName "Object"
                    TypeArguments = [] } } ]
          Superclass =
            { ClassName = ClassName "C0"
              TypeArguments =
                [ NonvariableType
                      { ClassName = ClassName "C0"
                        TypeArguments = [ TypeVariable(TypeVariableName "Y") ] } ] }
          Fields = []
          Constructor = { Parameters = [] }
          Methods = [] }

    let classC2 =
        { Name = ClassName "C2"
          TypeParameters =
            [ { Variance = Invariant
                Name = TypeVariableName "W"
                Bound =
                  { ClassName = ClassName "Object"
                    TypeArguments = [] } } ]
          Superclass =
            { ClassName = ClassName "C1"
              TypeArguments =
                [ NonvariableType
                      { ClassName = ClassName "C1"
                        TypeArguments = [ TypeVariable(TypeVariableName "W") ] } ] }
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

    let tester =
        { Name = ClassName "Tester"
          TypeParameters =
            [ { Variance = Invariant
                Name = TypeVariableName "X"
                Bound =
                  { ClassName = ClassName "N"
                    TypeArguments =
                      [ NonvariableType
                            { ClassName = ClassName "C2"
                              TypeArguments =
                                [ NonvariableType
                                      { ClassName = ClassName "T"
                                        TypeArguments = [] } ] } ] } }
              { Variance = Invariant
                Name = TypeVariableName "Y"
                Bound =
                  { ClassName = ClassName "Tester"
                    TypeArguments =
                      [ NonvariableType
                            { ClassName = ClassName "C2"
                              TypeArguments =
                                [ NonvariableType
                                      { ClassName = ClassName "N"
                                        TypeArguments =
                                          [ NonvariableType
                                                { ClassName = ClassName "T"
                                                  TypeArguments = [] } ] } ] }
                        TypeVariable(TypeVariableName "Y") ] } } ]
          Superclass =
            { ClassName = ClassName "Object"
              TypeArguments = [] }
          Fields = []
          Constructor = { Parameters = [] }
          Methods = [] }

    let classDefs = [ classN; classC0; classC1; classC2; classT; tester ]
