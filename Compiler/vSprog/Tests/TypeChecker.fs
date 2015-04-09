namespace vSprog.Tests

open FsUnit
open NUnit.Framework
open vSprog.Analysis
open vSprog.Parser
open vSprog.AST
open vSprog.CommonTypes

module TypeCheckerTest =

    let typecheckWith (input:string) (typecheckFun:AST -> Result<PrimitiveType>) : Result<PrimitiveType> =
        parse input "../../../fsharpTry/grammar.gram"
        >>= fun tree -> Success (toAST tree)
        |> fun ast -> match ast with
                      | Success ast' -> 
                            let result = typecheckFun ast'
                            printfn "%A" result
                            result
                      | Failure msg  -> failwith (String.concat "" msg)


    (* ------------------ Assignment ------------------ *)
                    
    [<Test>]
    let ``Assignment where lhs is int and rhs is int, expects to typecheck``() = 
        typecheckWith "let x:int := 2" typecheck
        |> should equal (Success (SimplePrimitive Primitive.Int))

    [<Test>]
    let ``Assignment where lhs is real and rhs is real, expects to typecheck``() = 
        typecheckWith "let x:real := 2.5" typecheck
        |> should equal (Success (SimplePrimitive Primitive.Real))


    // virker ikke lige nu. Det er noget med at den ikke ser det som samme type
    [<Test>]
    let ``Assignment where lhs is real and rhs is int, expects to NOT typecheck``() = 
        let res = typecheckWith "let x:real := 2" typecheck
        // explicit types are needed to make the test pass correctly
        let expect : Result<PrimitiveType> = (Failure ["Assignment does not typecheck"])
        res |> should equal expect 


    (* -------------------- If statement ---------------- *)
    // FIXME: Hvad blev vi enige med hinanden omkring if statements og typer?
    [<Test>]
    let ``If statement with condition of type Bool and body of type int, expects to typecheck with type int`` () =
        let res = typecheckWith "if ( true ) { 2 }" typecheck
        //|> printfn "%A"
        let expect : Result<PrimitiveType> = (Success (SimplePrimitive Primitive.Int))
        res |> should equal expect