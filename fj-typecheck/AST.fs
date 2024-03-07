module Thesis.AST

type FieldName = FieldName of string
type VariableName = VariableName of string
type MethodName = MethodName of string
type TypeName = TypeName of string
let typeString (TypeName typeString) = typeString

type Expression =
    | Var of VariableName
    | FieldAccess of Expression * FieldName
    | MethodInvocation of Expression * MethodName * Expression list
    | NewInstance of TypeName * Expression list

type Parameter = TypeName * VariableName

type Method =
    { ReturnType: TypeName
      MethodName: MethodName
      Parameters: Parameter list
      Return: Expression }

type Field = TypeName * FieldName

/// Returns the string inside the field's name
let fieldName ((_, FieldName fieldName): Field) = fieldName
/// Returns the string inside the field's type
let fieldType ((TypeName fieldType, _): Field) = fieldType

type Constructor =
    { ClassName: TypeName
      Fields: Field list }

type Class =
    { ClassName: TypeName
      SuperclassName: TypeName
      Fields: Field list
      Constructor: Constructor
      Methods: Method list }
