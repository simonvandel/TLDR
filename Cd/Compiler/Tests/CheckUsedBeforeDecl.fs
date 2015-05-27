namespace vSprog.Tests

open FsUnit
open NUnit.Framework
open vSprog.Tests.TestUtils
open vSprog.Analysis

module CheckUsedBeforeDeclTest = 

    
   ///////////////////////// Success expected //////////////////////

    [<Test>]
    let ``Declared before use (simple), expect Success``() = 
        let program = """let x:int := 2;
                         return x;"""
        let symTable = genSymTable program
        checkUsedBeforeDecl symTable
        |> expectSuccess
        |> run

    [<Test>]
    let ``Declared before use (if-statement), expect Success``() = 
        let program = """let x:int := 2;
                         if(x = 2) {2}"""
        let symTable = genSymTable program
        checkUsedBeforeDecl symTable
        |> expectSuccess
        |> run

    ///////////////////////// Failure expected //////////////////////
       
    [<Test>]
    let ``Used before declared simple, expect Failure``() = 
        let program = """return x + 1;"""
        let symTable = genSymTable program
        checkUsedBeforeDecl symTable
        |> expectFailure
        |> run

    [<Test>]
    let ``Used before declared if, expect Failure``() = 
        let program = """if(x = 2) {2};"""
        let symTable = genSymTable program
        checkUsedBeforeDecl symTable
        |> expectFailure
        |> run