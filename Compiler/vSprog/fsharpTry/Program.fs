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

    let input = "a := 2;"
    let dictionary = new Dictionary<string,SemanticAction>(dict [
                                                                 ("SimpleType", new SemanticAction(test));
                                                                 ("OnProgram", new SemanticAction(test))
                                                                 ])

    let parseResult = try
                          Some ((assembly.GetParser<String> (input,dictionary)).Parse())
                      with
                          | :? TargetInvocationException -> None
    
    match parseResult with
    | Some res when res.Errors.Count = 0 && res.IsSuccess -> 
      printfn "%s" "Parsing success. No errors"
      //printTree res.Root 0
      res.Root 
      |> createDecoratedAST
      |> fun dAST -> prettyPrintDecoratedAST dAST 0
    | Some res -> 
      // print all error messages
      res.Errors |> Seq.iter (fun err -> printfn "%O" (err))
    | None -> 
      printf "%s" "Error parsing. Have you defined all action codes in the dictionary?"

    let a = Console.ReadLine ()
    0 // return an integer exit code

