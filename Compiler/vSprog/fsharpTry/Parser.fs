namespace vSprog

open Hime.CentralDogma;
open Hime.Redist;
open vSprog.CommonTypes;
open System;

module Parser =

    let levelToString level : string =
        [0..level-1]
        |> Seq.collect (fun l -> sprintf "%s" "--")
        |> String.Concat

    let rec printTree (node:ASTNode) (level:int) = 
        levelToString level |> printf "%s" 

        printfn "%s" node.Symbol.Value

        for child in node.Children do
            printTree child (level+1)

    let parse (srcInput:string) : Result<ASTNode> =
        let task = CompilationTask ()
        task.AddInputFile "../../vSprogGrammar.gram"
        task.Mode <- Hime.CentralDogma.Output.Mode.Assembly
        let report = task.Execute()
        let assembly = SDK.AssemblyReflection "vSprogGrammar.dll"

        let parseResult = (assembly.GetParser<string> (srcInput)).Parse()

         
        if parseResult.Errors.Count = 0 && parseResult.IsSuccess then
            printTree parseResult.Root 0
            Success parseResult.Root
        else
            Failure (parseResult.Errors 
                     |> Seq.map (sprintf "%O")
                     |> List.ofSeq
                     )