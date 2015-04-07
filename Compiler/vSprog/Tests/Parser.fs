namespace vSprog.Tests

open FsUnit
open NUnit.Framework
open vSprog.Parser
open vSprog.AST
open vSprog.CommonTypes
open vSprog.ParserUtils

module ParserTest =

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
        <| should equal (Program [ (Constant (SimplePrimitive Primitive.Int, Int 2))])

    [<Test>]
    let ``When zero constant is given expect Int constant AST``() = 
        testParseWith "0"
        <| should equal (Program [ (Constant (SimplePrimitive Primitive.Int, Int 0)) ] )

    [<Test>]
    let ``When real constant is given, expect Real constant AST``() =
        testParseWith "2.5"
        <| should equal (Program [ (Constant (SimplePrimitive Primitive.Real, Real 2.5)) ])

    [<Test>]
    let ``When real constant is given starting with dot, expect Real constant AST``() =
        testParseWith ".5"
        <| should equal (Program [ (Constant (SimplePrimitive Primitive.Real, Real 0.5)) ])


    (* -------------------- Actor ---------------------- *)

    [<Test>]
    let ``When actor syntax is given with empty body, expect actor AST``() =
        debugTestParseWith "actor main := {}"
        <| should equal (Program [ (Actor ("main", Block []))])

    (* -------------------- Initialisation ---------------------- *)

    [<Test>]
    let ``When initialisation syntax is given with constant binding, expect initialisation AST with constant value``() =
        let lValue = {identity = "x"; isMutable = false; primitiveType = SimplePrimitive Primitive.Int;}
        let rhs = Constant (SimplePrimitive Primitive.Int, Int 5)
        testParseWith "let x:int := 5"
        <| should equal (Program [(Assignment (lValue, rhs))])

    [<Test>]
    let ``When initialisation syntax is given with variable binding, expect initialisation AST with constant value``() =
        let lValue = {identity = "x"; isMutable = true; primitiveType = SimplePrimitive Primitive.Int;}
        let rhs = Constant (SimplePrimitive Primitive.Int, Int 5)
        testParseWith "var x:int := 5"
        <| should equal (Program [(Assignment (lValue, rhs))])


    (* -------------------- Reassignment ---------------------- *)

    [<Test>]
    let ``When reassignment syntax is given with constant binding, expect reassignment AST with constant value``() =
        let rhs = Constant (SimplePrimitive Primitive.Int, Int 10000)
        debugTestParseWith "x := 10000"
        <| should equal (Program [(Reassignment (["x"], rhs))])

    [<Test>]
    let ``When reassignment syntax with accessor is given with constant binding, expect reassignment AST with constant value``() =
        let rhs = Constant (SimplePrimitive Primitive.Int, Int 10000)
        debugTestParseWith "x.y := 10000"
        <| should equal (Program [(Reassignment (["x"; "y"], rhs))])

    (* -------------------- If statements ---------------------- *)

    [<Test>]
    let ``When syntax for if statement is given with empty body, expect 'if' AST``() =
        let conditional = Constant (SimplePrimitive Primitive.Bool, Bool true)
        let body = Block []
        testParseWith "if ( true ) {}"
        <| should equal (Program [(If (conditional, body))])

    [<Test>]
    let ``When syntax for if statement is given with simple body, expect 'if' AST``() =
        let conditional = Constant (SimplePrimitive Primitive.Bool, Bool false)
        let assignInBody = Program [Assignment 
                                        ({identity = "x"; isMutable = true; primitiveType = SimplePrimitive Primitive.Int} // lvalue
                                        , Constant (SimplePrimitive Primitive.Int, Int 23))
                                   ]
                                  // value
        let body = Block [ assignInBody ]
        let expected = (Program [(If (conditional, body))])
        debugTestParseWith "if ( false ) {var x:int := 23}"
        <| should equal expected 


    (* -------------------- Struct ---------------------- *)

    [<Test>]
    let ``When syntax for struct is given with 0 fields, expect struct AST``() =
        testParseWith "struct structName := {}"
        <| should equal (Program [(Struct ("structName", []))])

    [<Test>]
    let ``When syntax for struct is given with 1 field, expect struct AST``() =
        let fieldInBlock = ("field1", SimplePrimitive Primitive.Int)
        testParseWith "struct structName := {field1:int}"
        <| should equal (Program [(Struct ("structName", [ fieldInBlock ]))])

    [<Test>]
    let ``When syntax for struct is given with 2 fields, expect struct AST``() =
        let field1InBlock = ("field1", SimplePrimitive Primitive.Int)
        let field2InBlock = ("field2", SimplePrimitive Primitive.Real)
        debugTestParseWith "struct structName := {field1:int; field2:real;}"
        <| should equal (Program [(Struct ("structName", [ field1InBlock; field2InBlock ]))])



    (* ---------------------------- Send -------------------------- *)

    [<Test>]
    let ``When syntax for send is given, expect Send AST`` () =
        debugTestParseWith "send actorHandle msg"
        <| should equal (Program [(Send ("actorHandle", "msg"))])


    (* --------------------------- Spawn --------------------------- *)

    [<Test>]
    let ``When syntax for spawn is given with immutable actor, expect Spawn AST`` () =
        let lhs = {identity = "actorHandle"; isMutable = false; primitiveType = UserType "actorName"}
        debugTestParseWith "let actorHandle:actorName := spawn actorName initMsg"
        <| should equal (Program [(Spawn (lhs, "actorName", "initMsg"))])

    [<Test>]
    let ``When syntax for spawn is given with mutable actor, expect Spawn AST`` () =
        let lhs = {identity = "actorHandle"; isMutable = true; primitiveType = UserType "actorName"}
        debugTestParseWith "var actorHandle:actorName := spawn actorName initMsg"
        <| should equal (Program [(Spawn (lhs, "actorName", "initMsg"))])

    (* --------------------------- Receive --------------------------- *)

    [<Test>]
    let ``When syntax for receive is given with empty body, expect Receive AST`` () =
        let msgType = SimplePrimitive Primitive.Int
        let body = Block []
        debugTestParseWith "receive msg:int := {}"
        <| should equal (Program [(Receive ("msg", msgType, body))])

    (* --------------------------- ForIn --------------------------- *)

    [<Test>]
    let ``When syntax for for-in-loop is given with empty body, expect ForIn AST`` () =
        let list = ListRange ([0;1;2] |> List.map (fun n -> Constant (SimplePrimitive Primitive.Int, Int n)))
        let body = Block []
        debugTestParseWith "for i in [0 .. 2] {}"
        <| should equal (Program [(ForIn ("i", list, body))])

    (* --------------------------- ListRange --------------------------- *)

    [<Test>]
    let ``When syntax for list from 0 to 3 is given, expect List AST`` () =
        let list = [0;1;2;3] |> List.map (fun n -> Constant (SimplePrimitive Primitive.Int, Int n))
        debugTestParseWith "[0 .. 3]"
        <| should equal (Program [(ListRange list)])

    [<Test>]
    let ``When syntax for list from -5 to 2 is given, expect List AST`` () =
        let list = [-5;-4;-3;-2;-1;0;1;2] |> List.map (fun n -> Constant (SimplePrimitive Primitive.Int, Int n))
        debugTestParseWith "[-5 .. 2]"
        <| should equal (Program [(ListRange list)])


    (* --------------------------- Operation --------------------------- *)

    [<Test>]
    let ``When syntax for operation is given, expect Operation AST`` () =
        let list = [0;1;2;3] |> List.map (fun n -> Constant (SimplePrimitive Primitive.Int, Int n))
        debugTestParseWith "2 + 3 + 4"
        <| should equal (Program [
                                    Operation (
                                                    Operation (
                                                                    Constant (SimplePrimitive Primitive.Int, Int 2),
                                                                    Plus,
                                                                    (Constant (SimplePrimitive Primitive.Int, Int 3)) 
                                                                    ),
                                                    Plus,
                                                    (Constant (SimplePrimitive Primitive.Int, Int 4))

                                               )
                                 ])