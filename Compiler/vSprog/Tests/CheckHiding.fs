namespace vSprog.Tests

open FsUnit
open NUnit.Framework
open vSprog.Parser
open vSprog.Analysis
open vSprog.CommonTypes
open vSprog.AST
open AnalysisUtils

module CheckHidingTest = 
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
                                 | Success sym -> sym = symTable
                                 | Failure _ -> false

    let expectFailure symTable = match checkHiding symTable with
                                 | Success _ -> false
                                 | Failure _ -> true
    
   ///////////////////////// Success expected //////////////////////

    [<Test>]
    let ``CheckHiding. No hiding, expect Success``() = 
        let program = """let x:int := 2;"""
        let symTable = genSymTable program
        run (expectSuccess symTable)

    [<Test>]
    let ``CheckHiding. No hiding in outer scope, expect Success``() = 
        let program = """let x:int := 2;
                         { let y:int := 1;}"""
        let symTable = genSymTable program
        run (expectSuccess symTable)

    ///////////////////////// Failure expected //////////////////////
       
    [<Test>]
    let ``CheckHiding. Hiding present, expect Failure``() = 
        let program = """let x:int := 2;
                         let x:real := 4.2"""
        let symTable = genSymTable program
        run (expectFailure symTable)

    [<Test>]
    let ``CheckHiding. Hiding present in outer scope, expect Failure``() = 
        let program = """let x:int := 2;
                         if(x = 2) {
                           let x:real := 4.2
                         }"""
        let symTable = genSymTable program
        run (expectFailure symTable)

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
        run (expectFailure symTable)

    [<Test>]
    let ``CheckHiding. hiding in block, expect Success``() = 
        let program = """let x:int := 2;
                         { let x:int := 1;}"""
        let symTable = genSymTable program
        run (expectFailure symTable)