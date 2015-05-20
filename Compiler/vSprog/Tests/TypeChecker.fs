namespace vSprog.Tests

open FsUnit
open NUnit.Framework
open vSprog.Analysis
open vSprog.Parser
open vSprog.AST
open vSprog.CommonTypes
open vSprog.TypeChecker
open AnalysisUtils
open vSprog.Tests.TestUtils

module TypeCheckerTest =
    let typecheckWith (input:string) (typecheckFun:AST -> SymbolTable -> Result<PrimitiveType>) : Result<PrimitiveType> =
        parse input "../../../fsharpTry/grammar.gram"
        >>= fun tree -> Success (toAST tree)
        |> fun ast -> match ast with
                      | Success ast' -> 
                            let result = typecheckFun ast' (evalState (buildSymbolTable ast') {symbolList = []; errors = []; scope = {outer = None; level = []}; scopeCounter = 0; ast = Program []}).symbolList
                            printfn "%A" result
                            result
                      | Failure msg  -> failwith (String.concat "" msg)


    (* ------------------ Initialisation ------------------ *)
                    
    [<Test>]
    let ``Initialisation where lhs is int and rhs is int, expects to typecheck``() = 
        typecheckWith "let x:int := 2;" checkTypes
        |> should equal (Success HasNoType)

    [<Test>]
    let ``Initialisation where lhs is real and rhs is real, expects to typecheck``() = 
        typecheckWith "let x:real := 2.5;" checkTypes
        |> should equal (Success HasNoType)

    [<Test>]
    let ``Initialisation where lhs is real and rhs is int, expects to have HasNoType``() = 
        typecheckWith "let x:real := 2;" checkTypes
        |> should equal (Success HasNoType)


    (* -------------------- If statement ---------------- *)
    [<Test>]
    let ``If statement with condition of type Bool and body of type int, expects to typecheck with type HasNoType`` () =
        let res = typecheckWith "if ( true ) { 2 }" checkTypes
        let expect : Result<PrimitiveType> = (Success HasNoType)
        res |> should equal expect

    (* -------------------- If-else statement ---------------- *)
    [<Test>]
    let ``If-else statement with condition of type Bool and body of type bool, expects to typecheck with type HasNoType`` () =
        let res = typecheckWith "if ( true ) { false } else { 5 }" checkTypes
        let expect : Result<PrimitiveType> = (Success HasNoType)
        res |> should equal expect