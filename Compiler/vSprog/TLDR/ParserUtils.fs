namespace vSprog

open Hime.CentralDogma;
open Hime.Redist;
open System;

module ParserUtils = 
    let levelToString level : string =
        [0..level-1]
        |> Seq.collect (fun l -> sprintf "%s" "-%")
        |> String.Concat

    let rec printTree (node:ASTNode) (level:int) = 
        levelToString level |> printf "%s" 

        printfn " %s" node.Symbol.Value

        for child in node.Children do
            printTree child (level+1)