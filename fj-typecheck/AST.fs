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
    { Name: string; Bound: NonvariableType }

type Field = Type * FieldName

type Parameter = Type * VariableName

type Expression =
    | Var of VariableName
    | FieldAccess of Expression * FieldName
    | MethodInvocation of Expression * MethodName * Type list * Expression list
    | NewInstance of NonvariableType * Expression list

type Constructor =
    { ClassName: TypeName
      Fields: Field list }

type Method =
    { Generics: TypeParameter list
      ReturnType: Type
      MethodName: MethodName
      Parameters: Parameter list
      Return: Expression }

type Class =
    { ClassName: TypeName
      Generics: TypeParameter list
      SuperclassName: TypeName
      Fields: Field list
      Constructor: Constructor
      Methods: Method list }

let typeString (TypeName typeString) = typeString

/// Returns the string inside the field's name
let fieldName ((_, FieldName fieldName): Field) = fieldName
// /// Returns the string inside the field's type
// let fieldType ((TypeName fieldType, _): Field) = fieldType
