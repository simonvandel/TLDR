namespace vSprog.Tests

open System
open vSprog.Parser
open vSprog.CommonTypes
open vSprog.Analysis
open vSprog.AST
open AnalysisUtils
open FsUnit
open NUnit.Framework

module TestUtils = 
    let genSymTable (prog:string) : SymbolTable =
        parse prog "../../../TLDR/grammar.gram"
        >>= fun tree -> Success (toAST tree)
        |> fun res -> match res with
                      | Success ast ->
                        (evalState (buildSymbolTable ast) {symbolList = []; errors = []; scope = {outer = None; level = []}; scopeCounter = 0; ast = Program []}).symbolList
                      | Failure _ -> failwith "failed in constructing ast"

    let run (test:bool) : unit =
        test |> should equal true

    let expectSuccess res = match res with
                                 | Success sym -> true
                                 | Failure _ -> false

    let expectFailure res = match res with
                                 | Success _ -> false
                                 | Failure _ -> true