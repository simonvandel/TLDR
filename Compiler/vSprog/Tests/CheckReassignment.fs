namespace vSprog.Tests

open FsUnit
open NUnit.Framework
open vSprog.Parser
open vSprog.Analysis
open vSprog.CommonTypes
open vSprog.AST
open AnalysisUtils
open vSprog.Tests.TestUtils

module CheckReassignmentTest =
    ///////////////////////// Success expected //////////////////////

    [<Test>]
    let ``valid, expect Success``() = 
        let program = """var x:int := 2;
                         x := 5;"""
        let symTable = genSymTable program
        checkReass symTable
        |> expectSuccess
        |> run

    [<Test>]
    let ``valid in different scope, expect Success``() = 
        let program = """var x:int := 2;
                         {
                           x := 5;
                         }"""
        let symTable = genSymTable program
        checkReass symTable
        |> expectSuccess
        |> run

    ///////////////////////// Failure expected //////////////////////

    [<Test>]
    let ``invalid, expect Failure``() = 
        let program = """let x:int := 2;
                         x := 5;"""
        let symTable = genSymTable program
        checkReass symTable
        |> expectFailure
        |> run

    [<Test>]
    let ``invalid in different scope, expect Failure``() = 
        let program = """let x:int := 2;
                         {
                           x := 5;
                         }"""
        let symTable = genSymTable program
        checkReass symTable
        |> expectFailure
        |> run