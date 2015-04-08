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
        testParseWith "2;"
        <| should equal (Program [ (Constant (SimplePrimitive Primitive.Int, Int 2))])

    [<Test>]
    let ``When zero constant is given expect Int constant AST``() = 
        testParseWith "0;"
        <| should equal (Program [ (Constant (SimplePrimitive Primitive.Int, Int 0)) ] )

    [<Test>]
    let ``When real constant is given, expect Real constant AST``() =
        testParseWith "2.5;"
        <| should equal (Program [ (Constant (SimplePrimitive Primitive.Real, Real 2.5)) ])

    [<Test>]
    let ``When real constant is given starting with dot, expect Real constant AST``() =
        testParseWith ".5;"
        <| should equal (Program [ (Constant (SimplePrimitive Primitive.Real, Real 0.5)) ])


    (* -------------------- Actor ---------------------- *)

    [<Test>]
    let ``When actor syntax is given with empty body, expect actor AST``() =
        debugTestParseWith "actor main := {};"
        <| should equal (Program [Actor (Identifier (SimpleIdentifier "main"), Block [])])

    (* -------------------- Initialisation ---------------------- *)

    [<Test>]
    let ``When initialisation syntax is given with constant binding, expect initialisation AST with constant value``() =
        let ident = Identifier (SimpleIdentifier "x")
        let lValue = {identity = ident; isMutable = false; primitiveType = ListPrimitive (SimplePrimitive Primitive.Int);}
        let rhs = Constant (ListPrimitive (SimplePrimitive Primitive.Char),PrimitiveValue.List [Char 'T'])
        debugTestParseWith "let x:[char] := \"T\""
        <| should equal (Program [(Assignment (lValue, rhs))])

    [<Test>]
    let ``When initialisation syntax is given with variable binding, expect initialisation AST with constant value``() =
        let ident = Identifier (SimpleIdentifier "x")
        let lValue = {identity = ident; isMutable = true; primitiveType = SimplePrimitive Primitive.Int;}
        let rhs = Constant (SimplePrimitive Primitive.Int, Int 5)
        testParseWith "var x:int := 5;"
        <| should equal (Program [(Assignment (lValue, rhs))])


    (* -------------------- Reassignment ---------------------- *)

    [<Test>]
    let ``When reassignment syntax is given with constant binding, expect reassignment AST with constant value``() =
        let rhs = Constant (SimplePrimitive Primitive.Int, Int 10000)
        debugTestParseWith "x := 10000;"
        <| should equal (Program [(Reassignment (Identifier(SimpleIdentifier "x"), rhs))])

    [<Test>]
    let ``When reassignment syntax with accessor is given with constant binding, expect reassignment AST with constant value``() =
        let rhs = Constant (SimplePrimitive Primitive.Int, Int 10000)
        debugTestParseWith "x.y := 10000;"
        <| should equal (Program [(Reassignment (Identifier (IdentifierAccessor ["x"; "y"]), rhs))])

    [<Test>]
    let ``When reassignment syntax with accessor is given with identifer.accessor, expect reassignment AST with constant value``() =
        let rhs = Identifier ( IdentifierAccessor ["a"; "b"] )
        debugTestParseWith "x.y := a.b;"
        <| should equal (Program [(Reassignment (Identifier (IdentifierAccessor ["x"; "y"]), rhs))])

    [<Test>]
    let ``When reassignment syntax with accessor is given with identifer.accessor.accessor, expect reassignment AST with constant value``() =
        let rhs = Identifier ( IdentifierAccessor ["a"; "b"; "c"] )
        debugTestParseWith "x.y := a.b.c;"
        <| should equal (Program [(Reassignment (Identifier (IdentifierAccessor ["x"; "y"]), rhs))])

    (* -------------------- If statements ---------------------- *)

    [<Test>]
    let ``When syntax for if statement is given with empty body, expect 'if' AST``() =
        let conditional = Constant (SimplePrimitive Primitive.Bool, Bool true)
        let body = Block []
        testParseWith "if ( true ) {}"
        <| should equal (Program [(If (conditional, body))])

    [<Test>]
    let ``When syntax for if statement is given with simple body, expect 'if' AST``() =
        let ident = Identifier (SimpleIdentifier "x")
        let conditional = Constant (SimplePrimitive Primitive.Bool, Bool false)
        let assignInBody = Program [Assignment 
                                        ({identity = ident; isMutable = true; primitiveType = SimplePrimitive Primitive.Int} // lvalue
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
        debugTestParseWith "struct structName := {field1:int}"
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
        let ident = Identifier (SimpleIdentifier "actorHandle")
        let lhs = {identity = ident; isMutable = false; primitiveType = UserType "actorName"}
        let actorType = Identifier (SimpleIdentifier "actorName")
        let initMsg = Identifier (SimpleIdentifier "initMsg")
        debugTestParseWith "let actorHandle:actorName := spawn actorName initMsg"
        <| should equal (Program [(Spawn (lhs, actorType, initMsg))])

    [<Test>]
    let ``When syntax for spawn is given with mutable actor, expect Spawn AST`` () =
        let ident = Identifier (SimpleIdentifier "actorHandle")
        let lhs = {identity = ident; isMutable = true; primitiveType = UserType "actorName"}
        let actorType = Identifier (SimpleIdentifier "actorName")
        let initMsg = Identifier (SimpleIdentifier "initMsg")
        debugTestParseWith "var actorHandle:actorName := spawn actorName initMsg"
        <| should equal (Program [(Spawn (lhs, actorType, initMsg))])

    (* --------------------------- Receive --------------------------- *)

    [<Test>]
    let ``When syntax for receive is given with empty body, expect Receive AST`` () =
        let msgType = SimplePrimitive Primitive.Int
        let body = Block []
        debugTestParseWith "receive msg:int := {}"
        <| should equal (Program [(Receive ("msg", msgType, body))])

    [<Test>]
    let ``When syntax for receive is given with msg of usertype and empty body, expect Receive AST`` () =
        let msgType = UserType "msgType"
        let body = Block []
        debugTestParseWith "receive msg:msgType := {}"
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

    [<Test>]
    let ``When syntax for operation is given with modulo and equals, expect Operation AST`` () =
        debugTestParseWith "i % 20 = 0"
        <| should equal (Program [
                                    Operation (
                                                    Operation (
                                                                    Identifier (SimpleIdentifier "i"),
                                                                    Modulo,
                                                                    (Constant (SimplePrimitive Primitive.Int, Int 20)) 
                                                                    ),
                                                    Equals,
                                                    (Constant (SimplePrimitive Primitive.Int, Int 0))
                                               )
                                 ])


    (* --------------------------- Functions --------------------------- *)
    [<Test>]
    let ``When syntax for function with zero arguments, expect Function AST`` () =
        let body = Block [ Program [ Constant (SimplePrimitive Primitive.Int, Int 5) ] ]
        debugTestParseWith "let func1() : int := {5}"
        <| should equal (Program [
                                    Function ("func1", [], [SimplePrimitive Primitive.Int], body)
                                 ])

    [<Test>]
    let ``When syntax for function with 1 argument, expect Function AST`` () =
        let body = Block [ Program [ Identifier (SimpleIdentifier "x") ] ]
        debugTestParseWith "let func1(x) : int -> int := {x}"
        <| should equal (Program [
                                    Function ("func1", ["x"], [SimplePrimitive Primitive.Int; SimplePrimitive Primitive.Int], body)
                                 ])

    [<Test>]
    let ``When syntax for function with 2 arguments, expect Function AST`` () =
        let body = Block [ Program [ Identifier (SimpleIdentifier "x") ] ]
        debugTestParseWith "let func2(x, y) : int -> int -> int := {x}"
        <| should equal (Program [
                                    Function (
                                        "func2", 
                                        ["x"; "y"], 
                                        [SimplePrimitive Primitive.Int; SimplePrimitive Primitive.Int; SimplePrimitive Primitive.Int], 
                                        body
                                        )
                                 ])

    (* --------------------------- Function Invocation --------------------------- *)
    [<Test>]
    let ``When syntax for function invocation with 0 parameters, expect Function invocation AST`` () =
        debugTestParseWith "f()"
        <| should equal (Program [ Invocation ("f",[]) ])


    [<Test>]
    let ``When syntax for function invocation with 1 userdefined parameter, expect Function invocation AST`` () =
        debugTestParseWith "f(x)"
        <| should equal (Program [ Invocation ("f",["x"]) ])

    (* --------------------------- Struct Literal --------------------------- *)
    [<Test>]
    let ``When syntax for struct literal with 1 field, expect Struct literal AST`` () =
        let fieldName1 = Identifier (SimpleIdentifier "field1")
        let struct1 = StructLiteral [(fieldName1, Constant (SimplePrimitive Primitive.Int, Int 5))]
        debugTestParseWith "(field1 := 5)"
        <| should equal (Program [ struct1  ])

    [<Test>]
    let ``When syntax for struct literal with 2 fields, expect Struct literal AST`` () =
        let fieldName1 = Identifier (SimpleIdentifier "field1")
        let fieldName2 = Identifier (SimpleIdentifier "field2")
        let struct1 = StructLiteral [
                                        (fieldName1, Constant (SimplePrimitive Primitive.Int, Int 5));
                                        (fieldName2, Constant (SimplePrimitive Primitive.Int, Int 10));
                                    ]
        debugTestParseWith "(field1 := 5; field2 := 10)"
        <| should equal (Program [ struct1  ])

    (* --------------------------- String literal --------------------------- *)
    [<Test>]
    let ``When syntax for string literal, expect string literal AST`` () =
        let fieldName1 = Identifier (SimpleIdentifier "field1")
        let struct1 = StructLiteral [(fieldName1, Constant (SimplePrimitive Primitive.Int, Int 5))]
        debugTestParseWith "\"Tub\""
        <| should equal (Program [ Constant (ListPrimitive (SimplePrimitive Primitive.Char), PrimitiveValue.List [Char 'T'; Char 'u'; Char 'b'])  ])