namespace vSprog.Tests

open FsUnit
open NUnit.Framework
open vSprog.Parser
open vSprog.AST
open vSprog.CommonTypes
open vSprog.ParserUtils

module AST =

    let debugTestParseWith (input:string) (test:AST -> unit) : unit =
        parse input "../../../fsharpTry/grammar.gram"
        >>= fun tree -> 
            printTree tree 0
            Success (toAST tree)
        |> fun ast -> match ast with
                      | Success ast' -> 
                            printfn "%A" ast'
                            test ast'
                      | Failure msg  -> failwith (String.concat "" msg)

    let testParseWith (input:string) (test:AST -> unit) : unit =
        parse input "../../../fsharpTry/grammar.gram"
        >>= fun tree -> Success (toAST tree)
        |> fun ast -> match ast with
                      | Success ast' -> test ast'
                      | Failure msg  -> failwith (String.concat "" msg)

    (* -------------------- Ints and Reals --------------------------- *)
    
    [<Test>]
    let ``When int constant is given expect Int constant AST``() = 
        testParseWith "2"
        <| should equal (Constant (SimplePrimitive Primitive.Int, Int 2))

    [<Test>]
    let ``When zero constant is given expect Int constant AST``() = 
        testParseWith "0"
        <| should equal (Constant (SimplePrimitive Primitive.Int, Int 0))

    [<Test>]
    let ``When real constant is given, expect Real constant AST``() =
        testParseWith "2.5"
        <| should equal (Constant (SimplePrimitive Primitive.Real, Real 2.5))

    [<Test>]
    let ``When real constant is given starting with dot, expect Real constant AST``() =
        testParseWith ".5"
        <| should equal (Constant (SimplePrimitive Primitive.Real, Real 0.5))


    (* -------------------- Actor ---------------------- *)

    [<Test>]
    let ``When actor syntax is given with empty body, expect actor AST``() =
        testParseWith "actor main := {}"
        <| should equal (Actor "main")

    (* -------------------- Initialisation ---------------------- *)

    [<Test>]
    let ``When initialisation syntax is given with constant binding, expect initialisation AST with constant value``() =
        let lValue = {identity = "x"; isMutable = false; primitiveType = SimplePrimitive Primitive.Int;}
        let rhs = Constant (SimplePrimitive Primitive.Int, Int 5)
        testParseWith "let x:int := 5"
        <| should equal (Assignment (lValue, rhs))

    [<Test>]
    let ``When initialisation syntax is given with variable binding, expect initialisation AST with constant value``() =
        let lValue = {identity = "x"; isMutable = true; primitiveType = SimplePrimitive Primitive.Int;}
        let rhs = Constant (SimplePrimitive Primitive.Int, Int 5)
        testParseWith "var x:int := 5"
        <| should equal (Assignment (lValue, rhs))