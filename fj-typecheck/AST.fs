module AST

type MethodName = MethodName of string
type ClassName = ClassName of string
type TypeVariableName = TypeVariableName of string
type FieldName = FieldName of string
type ParameterName = ParameterName of string

type Type =
    | TypeVariable of TypeVariableName
    | NonvariableType of NonvariableType

and NonvariableType =
    { ClassName: ClassName
      TypeArguments: Type list }

type Variance =
    | Invariant
    | Covariant
    | Contravariant

type TypeParameter =
    { Variance: Variance
      Name: TypeVariableName
      Bound: NonvariableType }

type Field = { Type: Type; Name: FieldName }

type Parameter = { Type: Type; Name: ParameterName }

type Expression =
    | Variable of string
    | FieldAccess of FieldAccess
    | MethodInvocation of MethodInvocation
    | NewInstance of NewInstance

and FieldAccess =
    { Object: Expression; Field: FieldName }

and MethodInvocation =
    { Object: Expression
      Method: MethodName
      TypeArguments: Type list
      Arguments: Expression list }

and NewInstance =
    { Class: NonvariableType
      Parameters: Expression list }

type Constructor = { Parameters: Parameter list }

type Method =
    { TypeParameters: TypeParameter list
      ReturnType: Type
      Name: MethodName
      Parameters: Parameter list
      Return: Expression }

type Class =
    { Name: ClassName
      TypeParameters: TypeParameter list
      Superclass: NonvariableType
      Fields: Field list
      Constructor: Constructor
      Methods: Method list }
