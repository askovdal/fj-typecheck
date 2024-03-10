module Thesis.AST

type FieldName = FieldName of string
type VariableName = VariableName of string
type MethodName = MethodName of string
type TypeName = TypeName of string
type TypeVariableName = TypeVariableName of string

type Type =
    | TypeVariable of TypeVariableName
    | NonvariableType of NonvariableType

and NonvariableType =
    { ClassName: TypeName
      Generics: Type list }

/// Returns a nonvariable type with no generics
let boringType className =
    NonvariableType
        { ClassName = TypeName className
          Generics = [] }

type TypeParameter =
    { Name: TypeVariableName
      Bound: NonvariableType }

type Field = Type * FieldName

type Parameter = Type * VariableName

type Expression =
    | Var of VariableName
    | FieldAccess of Expression * FieldName
    | MethodInvocation of Expression * MethodName * Type list * Expression list
    | NewInstance of NonvariableType * Expression list

type Constructor = { Fields: Field list }

type Method =
    { Generics: TypeParameter list
      ReturnType: Type
      MethodName: MethodName
      Parameters: Parameter list
      Return: Expression }

type Class =
    { ClassName: TypeName
      Generics: TypeParameter list
      Superclass: NonvariableType
      Fields: Field list
      Constructor: Constructor
      Methods: Method list }

let typeNameString (TypeName typeName) = typeName
let typeVariableNameString (TypeVariableName typeVariableName) = typeVariableName
