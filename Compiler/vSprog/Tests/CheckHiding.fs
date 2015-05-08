namespace vSprog.Tests

open FsUnit
open NUnit.Framework
open vSprog.Tests.TestUtils
open vSprog.Analysis

module CheckHidingTest = 

    
   ///////////////////////// Success expected //////////////////////

    [<Test>]
    let ``CheckHiding. No hiding, expect Success``() = 
        let program = """let x:int := 2;"""
        let symTable = genSymTable program
        checkHiding symTable
        |> expectSuccess
        |> run

    [<Test>]
    let ``CheckHiding. No hiding in outer scope, expect Success``() = 
        let program = """let x:int := 2;
                         { let y:int := 1;}"""
        let symTable = genSymTable program
        checkHiding symTable
        |> expectSuccess
        |> run

    ///////////////////////// Failure expected //////////////////////
       
    [<Test>]
    let ``CheckHiding. Hiding present, expect Failure``() = 
        let program = """let x:int := 2;
                         let x:real := 4.2"""
        let symTable = genSymTable program
        checkHiding symTable
        |> expectFailure
        |> run

    [<Test>]
    let ``CheckHiding. Hiding present in outer scope, expect Failure``() = 
        let program = """let x:int := 2;
                         if(x = 2) {
                           let x:real := 4.2
                         }"""
        let symTable = genSymTable program
        checkHiding symTable
        |> expectFailure
        |> run

    [<Test>]
    let ``CheckHiding. Hiding present in outer, outer scope, expect Failure``() = 
        let program = """let x:int := 2;
                         if(x = 2) {
                           let a:real := 4.2;
                           if(a = 0) {
                             let x:real := 2.0;
                           }
                         }"""
        let symTable = genSymTable program
        checkHiding symTable
        |> expectFailure
        |> run

    [<Test>]
    let ``CheckHiding. hiding in block, expect Failure``() = 
        let program = """let x:int := 2;
                         { let x:int := 1;}"""
        let symTable = genSymTable program
        checkHiding symTable
        |> expectFailure
        |> run