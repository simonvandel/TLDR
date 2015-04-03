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
        <| should equal (Actor ("main", Block []))

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

    (* -------------------- If statements ---------------------- *)

    [<Test>]
    let ``When syntax for if statement is given with empty body, expect 'if' AST``() =
        let conditional = Constant (SimplePrimitive Primitive.Bool, Bool true)
        let body = Block []
        testParseWith "if ( true ) {}"
        <| should equal (If (conditional, body))

    [<Test>]
    let ``When syntax for if statement is given with simple body, expect 'if' AST``() =
        let conditional = Constant (SimplePrimitive Primitive.Bool, Bool false)
        let assignInBody = Assignment 
                            ({identity = "x"; isMutable = true; primitiveType = SimplePrimitive Primitive.Int} // lvalue
                            , Constant (SimplePrimitive Primitive.Int, Int 23)) // value
        let body = Block [ assignInBody ]
        testParseWith "if ( false ) {var x:int := 23}"
        <| should equal (If (conditional, body))


    (* -------------------- Struct ---------------------- *)

    [<Test>]
    let ``When syntax for struct is given with 0 fields, expect struct AST``() =
        testParseWith "struct structName := {}"
        <| should equal (Struct ("structName", []))

    [<Test>]
    let ``When syntax for struct is given with 1 field, expect struct AST``() =
        let fieldInBlock = ("field1", SimplePrimitive Primitive.Int)
        testParseWith "struct structName := {field1:int}"
        <| should equal (Struct ("structName", [ fieldInBlock ]))

    [<Test>]
    let ``When syntax for struct is given with 2 fields, expect struct AST``() =
        let field1InBlock = ("field1", SimplePrimitive Primitive.Int)
        let field2InBlock = ("field2", SimplePrimitive Primitive.Real)
        testParseWith "struct structName := {field1:int; field2:real}"
        <| should equal (Struct ("structName", [ field1InBlock; field2InBlock ]))