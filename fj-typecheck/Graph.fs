module Graph

open AST
open ClassTable
open Utils

type Node = TypeVariableName

type EdgeType =
    | Expansive
    | NonExpansive

type Edge = { To: Node; Type: EdgeType }

type Graph = Map<Node, Set<Edge>>

let findAllCycles (graph: Graph) =
    let rec dfs currentPath node =
        if currentPath |> List.exists (fun (n, _) -> n = node) then
            // Create cycle from current path
            let cycle =
                (currentPath |> List.takeWhile (fun (n, _) -> n <> node))
                @ [ (node, currentPath |> List.find (fun (n, _) -> n = node) |> snd) ]
                |> List.rev

            [ cycle ]
        else
            let edges = Map.tryFind node graph |> Option.defaultValue Set.empty

            edges
            |> Set.toList
            |> List.collect (fun edge -> dfs ((node, edge.Type) :: currentPath) edge.To)

    graph |> Map.toList |> List.collect (fun (node, _) -> dfs [] node)

let addEdge (fromNode: Node) (toNode: Node) (edgeType: EdgeType) (graph: Graph) =
    graph
    |> Map.change fromNode (function
        | Some edges -> Some(edges |> Set.add { To = toNode; Type = edgeType })
        | None -> Some(Set.singleton { To = toNode; Type = edgeType }))

let rec addEdges (classTable: ClassTable) (term: NonvariableType) (typeParam: TypeVariableName) (graph: Graph) =
    let folder (result: bool, graph: Graph) (index: int, typeArgument: Type) : bool * Graph =
        match typeArgument with
        | TypeVariable typeVariableName when typeVariableName = typeParam ->
            match classTable |> ClassTable.tryFind term.ClassName with
            | None -> failwith $"Class {term.ClassName |> classNameString} not defined"
            | Some classDef ->
                let termTypeParam = (classDef.TypeParameters |> List.item index)
                (true, graph |> addEdge typeParam termTypeParam.Name NonExpansive)

        | NonvariableType nonvariableType ->
            match addEdges classTable nonvariableType typeParam graph with
            | true, updatedGraph ->
                match classTable |> ClassTable.tryFind term.ClassName with
                | None -> failwith $"Class {term.ClassName |> classNameString} not defined"
                | Some classDef ->
                    let termTypeParam = (classDef.TypeParameters |> List.item index)
                    (result, updatedGraph |> addEdge typeParam termTypeParam.Name Expansive)

            | false, updatedGraph -> (result, updatedGraph)

        | _ -> (result, graph)

    ((false, graph), (term.TypeArguments |> List.indexed)) ||> List.fold folder

let typeParamFolder (classTable: ClassTable) (superClass: NonvariableType) (graph: Graph) (typeParam: TypeParameter) =
    let _, updatedGraph = graph |> addEdges classTable superClass typeParam.Name
    updatedGraph

let classFolder (classTable: ClassTable) (graph: Graph) (classDef: Class) =
    (graph, classDef.TypeParameters)
    ||> List.fold (typeParamFolder classTable classDef.Superclass)

let createGraph (classTable: ClassTable) (classDefs: Class list) =
    (Map.empty, classDefs) ||> List.fold (classFolder classTable)

let classTableExpansive (classTable: ClassTable) (classDefs: Class list) =
    createGraph classTable classDefs
    |> findAllCycles
    |> List.exists (List.exists (fun (_, edgeType) -> edgeType = Expansive))
