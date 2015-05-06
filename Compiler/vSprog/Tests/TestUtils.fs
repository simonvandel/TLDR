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
        parse prog "../../../fsharpTry/grammar.gram"
        >>= fun tree -> Success (toAST tree)
        |> fun res -> match res with
                      | Success ast ->
                        (evalState (buildSymbolTable ast) {symbolList = []; errors = []; scope = {outer = None; level = []}; scopeCounter = 0}).symbolList
                      | Failure _ -> failwith "failed in constructing ast"

    let run (test:bool) : unit =
        test |> should equal true

    let expectSuccess symTable = match checkHiding symTable with
                                 | Success sym -> true
                                 | Failure _ -> false

    let expectFailure symTable = match checkHiding symTable with
                                 | Success _ -> false
                                 | Failure _ -> true