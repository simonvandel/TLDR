open Hime.CentralDogma;
open Hime.Redist;
open System;
open System.Collections.Generic;
open System.Reflection;

let test (head:Symbol) (body:SemanticBody) = ()

type NodeType =
    | Int
    | Bool
    | NotYetChecked
    // TODO: Tilføj flere typer

type DecoratedASTNode =
    {
        Symbol:Symbol
        Position:TextPosition
        Children:ASTFamily
        NodeType:NodeType
    }

type DecoratedAST =
    | Leaf of DecoratedASTNode
    | Tree of DecoratedAST list

type Val =
    | Int of int
    | Real of float
    | Char of char
    | Bool of bool
    | List of Val list

type TypeSymbol = String * Val // (id, value)

type Scope = 
    {
        SymbolList:TypeSymbol list
        Parent:Scope
    }

type Ex =
    | Variable of string * string // * Scope // (name, type)
    | Int of int
    | Real of float

type AST = 
    | Program of AST list
    //| Assignment of TypeSymbol * Scope
    | Assignment of Ex * Ex

let toInternalEx (ast:ASTNode) : Ex =
    match ast.Symbol.Value with
    | value when System.Int32.TryParse(value,ref 0) -> Int (System.Int32.Parse(value))
    | value when System.Double.TryParse(value,ref 0.0) -> Real (float value)
    | value -> // must be variable
        let id = value
        let typeId = (ast.Children.Item 0).Symbol.Value
        Variable (id, typeId)

let toInternalAST (ast:ASTNode) : AST =
    match ast.Symbol.Value with
    | "Statement" -> 
        match (ast.Children.Item 0).Symbol.Value with
        | "Initialisation" -> 
            let curNode = ast.Children.Item 0
            let lhs = toInternalEx (curNode.Children.Item 1)
            let rhs = toInternalEx (curNode.Children.Item 2)

            Assignment(lhs, rhs)
        | _ -> 
            printf "%s" "Var andet end declaration"
            Assignment(Int 1, Int 1)
    | _ -> Assignment(Int 0, Int 0)


let levelToString level : String =
    [0..level-1]
    |> Seq.collect (fun l -> sprintf "%s" "--")
    |> String.Concat

let rec printTree (node:ASTNode) (level:int) = 
    levelToString level |> printf "%s" 

    printfn "%s" node.Symbol.Value

    for child in node.Children do
        printTree child (level+1)


let rec createDecoratedAST (ast:ASTNode) : DecoratedAST =
    match ast with
    | ast when ast.Children.Count = 0 -> 
        Leaf ({Symbol = ast.Symbol; Position = ast.Position; Children = ast.Children; NodeType = NotYetChecked })
    | ast -> Tree ( List.ofSeq (seq { for i in ast.Children -> createDecoratedAST i}) )

let rec prettyPrintDecoratedAST dAST (level:int) =
    levelToString level |> printfn "%s" 

    printfn "%A" dAST


[<EntryPoint>]
let main argv = 

    let task = CompilationTask ()
    task.AddInputFile "../../vSprogGrammar.gram"
    task.Mode <- Hime.CentralDogma.Output.Mode.Assembly
    let report = task.Execute()
    let assembly = SDK.AssemblyReflection "vSprogGrammar.dll"

    let input = "let a:int := 2.5;"
    let dictionary = new Dictionary<string,SemanticAction>(dict [
                                                                 ("SimpleType", new SemanticAction(test));
                                                                 ("OnProgram", new SemanticAction(test))
                                                                 ])

    let parseResult = try
                          Some ((assembly.GetParser<String> (input)).Parse())
                      with
                          | :? TargetInvocationException -> None
    
    match parseResult with
    | Some res when res.Errors.Count = 0 && res.IsSuccess -> 
      printfn "%s" "Parsing success. No errors"
      //printTree res.Root 0
      res.Root 
      |> fun r -> printTree r 0

      res.Root
      |> toInternalAST
      |> printf "%A"
      //|> createDecoratedAST
      //|> fun dAST -> prettyPrintDecoratedAST dAST 0
    | Some res -> 
      // print all error messages
      res.Errors |> Seq.iter (fun err -> printfn "%O" (err))
    | None -> 
      printf "%s" "Error parsing. Have you defined all action codes in the dictionary?"

    let a = Console.ReadLine ()
    0 // return an integer exit code

