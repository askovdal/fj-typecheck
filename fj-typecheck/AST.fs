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
      Generics: Type list }

let rec debugType (typeDef: Type) =
    match typeDef with
    | TypeVariable(TypeVariableName name) -> name
    | NonvariableType { ClassName = ClassName name
                        Generics = generics } ->
        let genericsString = generics |> List.map debugType |> String.concat ", "

        if genericsString = "" then
            name
        else
            $"{name}<{genericsString}>"

/// Returns a nonvariable type with no generics
let boringType className =
    NonvariableType
        { ClassName = ClassName className
          Generics = [] }

type TypeParameter =
    { Name: TypeVariableName
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
      Generics: Type list
      Arguments: Expression list }

and NewInstance =
    { Class: NonvariableType
      Parameters: Expression list }

type Constructor = { Parameters: Parameter list }

type Method =
    { Generics: TypeParameter list
      ReturnType: Type
      Name: MethodName
      Parameters: Parameter list
      Return: Expression }

type Class =
    { Name: ClassName
      Generics: TypeParameter list
      Superclass: NonvariableType
      Fields: Field list
      Constructor: Constructor
      Methods: Method list }

type ClassTable = Map<ClassName, Class>

let isObject = (=) (ClassName "Object")

module ClassTable =
    let empty: ClassTable = Map.empty

    let addClasses (classDefs: Class list) (classTable: ClassTable) =
        let addClass (classTable: ClassTable) (classDef: Class) =
            classTable |> Map.add classDef.Name classDef

        (classTable, classDefs) ||> List.fold addClass

    let containsClass (className: ClassName) (classTable: ClassTable) =
        className |> isObject || classTable |> Map.containsKey className

    let tryFind (className: ClassName) (classTable: ClassTable) : Class option =
        if className |> isObject then
            let objectClass =
                { Name = ClassName "Object"
                  Generics = []
                  Superclass =
                    { ClassName = ClassName "Object"
                      Generics = [] }
                  Fields = []
                  Constructor = { Parameters = [] }
                  Methods = [] }

            Some objectClass
        else
            classTable |> Map.tryFind className

type State = Class * ClassTable

let classNameString (ClassName className) = className
let typeVariableNameString (TypeVariableName typeVariableName) = typeVariableName
let fieldNameString (FieldName fieldName) = fieldName
let parameterNameString (ParameterName parameterName) = parameterName


let parameterToField
    ({ Type = typeDef
       Name = ParameterName name }: Parameter)
    : Field =
    { Type = typeDef
      Name = FieldName name }

let fieldToParameter
    ({ Type = typeDef
       Name = FieldName name }: Field)
    : Parameter =
    { Type = typeDef
      Name = ParameterName name }
