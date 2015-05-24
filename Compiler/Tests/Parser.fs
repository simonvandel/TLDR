namespace vSprog.Tests

open FsUnit
open NUnit.Framework
open vSprog.Parser
open vSprog.AST
open vSprog.CommonTypes
open vSprog.ParserUtils


module ParserTest =

    let debugTestParseWith (input:string) (test:AST -> unit) : unit =
        parse input "../../Compiler/grammar.gram"
        >>= fun tree -> 
            printTree tree 0
            Success (toAST tree)
        |> fun ast -> match ast with
                      | Success ast' -> 
                            printfn "%A" ast'
                            test ast'
                      | Failure msg  -> failwith (String.concat "" msg)

    let testParseWith (input:string) (test:AST -> unit) : unit =
        parse input "../../Compiler/grammar.gram"
        >>= fun tree -> Success (toAST tree)
        |> fun ast -> match ast with
                      | Success ast' -> test ast'
                      | Failure msg  -> failwith (String.concat "" msg)

    (* -------------------- Constants --------------------------- *)
    
    [<Test>]
    let ``When int constant is given expect Int constant AST``() = 
        testParseWith "2;"
        <| should equal (Program [Body [ (Constant (SimplePrimitive Primitive.Int, PrimitiveValue.Int 2))]])

    [<Test>]
    let ``When zero constant is given expect Int constant AST``() = 
        testParseWith "0;"
        <| should equal (Program [Body [ (Constant (SimplePrimitive Primitive.Int, PrimitiveValue.Int 0)) ]] )

    [<Test>]
    let ``When real constant is given, expect Real constant AST``() =
        testParseWith "2.5;"
        <| should equal (Program [Body [ (Constant (SimplePrimitive Primitive.Real, PrimitiveValue.Real 2.5)) ]])

    [<Test>]
    let ``When real constant is given starting with dot, expect Real constant AST``() =
        testParseWith ".5;"
        <| should equal (Program [Body [ (Constant (SimplePrimitive Primitive.Real, PrimitiveValue.Real 0.5)) ]])


    (* -------------------- Actor ---------------------- *)

    [<Test>]
    let ``When actor syntax is given with empty body, expect actor AST``() =
        debugTestParseWith "actor main := {};"
        <| should equal (Program [Body [Actor ("main", Block [])]])

    (* -------------------- Initialisation ---------------------- *)

    [<Test>]
    let ``When initialisation syntax is given with constant binding, expect initialisation AST with constant value``() =
        let ident = SimpleIdentifier "x"
        let lValue = {identity = ident; isMutable = false; primitiveType = ListPrimitive (SimplePrimitive Primitive.Char, 0);}
        let rhs = Constant (ListPrimitive (SimplePrimitive Primitive.Char, 1),PrimitiveValue.List [PrimitiveValue.Char 'T'])
        debugTestParseWith "let x:[char] := \"T\""
        <| should equal (Program [Body [(Initialisation (lValue, rhs))]])

    [<Test>]
    let ``When initialisation syntax is given with variable binding, expect initialisation AST with constant value``() =
        let ident = SimpleIdentifier "x"
        let lValue = {identity = ident; isMutable = true; primitiveType = SimplePrimitive Primitive.Int;}
        let rhs = Constant (SimplePrimitive Primitive.Int, PrimitiveValue.Int 5)
        debugTestParseWith "var x:int := 5;"
        <| should equal (Program [Body [(Initialisation (lValue, rhs))]])


    (* -------------------- Reassignment ---------------------- *)

    [<Test>]
    let ``When reassignment syntax is given with constant binding, expect reassignment AST with constant value``() =
        let rhs = Constant (SimplePrimitive Primitive.Int, PrimitiveValue.Int 10000)
        debugTestParseWith "x := 10000;"
        <| should equal (Program [Body [(Reassignment (SimpleIdentifier "x", rhs))]])

    [<Test>]
    let ``When reassignment syntax with accessor is given with constant binding, expect reassignment AST with constant value``() =
        let rhs = Constant (SimplePrimitive Primitive.Int, PrimitiveValue.Int 10000)
        debugTestParseWith "x.y := 10000;"
        <| should equal (Program [Body [(Reassignment (IdentifierAccessor ("x", Identifier (SimpleIdentifier "y", HasNoType)), rhs))]])

    [<Test>]
    let ``When reassignment syntax with accessor is given with identifer.accessor, expect reassignment AST with constant value``() =
        let rhs = Identifier ( IdentifierAccessor ("a", Identifier (SimpleIdentifier "b", HasNoType)), HasNoType )
        debugTestParseWith "x.y := a.b;"
        <| should equal (Program [Body [(Reassignment (IdentifierAccessor ("x", Identifier (SimpleIdentifier "y", HasNoType)), rhs))]])

    [<Test>]
    let ``When reassignment syntax with accessor is given with identifer.accessor.accessor, expect reassignment AST with constant value``() =
        let lhs = IdentifierAccessor ("x", (Identifier (SimpleIdentifier "y", HasNoType)))
        let cId = Identifier ( SimpleIdentifier "c", HasNoType )   
        let bc = IdentifierAccessor ("b", cId)
        let bcId = Identifier (bc, HasNoType)
        let abc = IdentifierAccessor ("a", bcId)
        let rhs = Identifier (abc, HasNoType)
        debugTestParseWith "x.y := a.b.c;"
        <| should equal (Program [Body [(Reassignment (lhs, rhs))]])

    (* -------------------- If statements ---------------------- *)

    [<Test>]
    let ``When syntax for if statement is given with empty body, expect 'if' AST``() =
        let conditional = Constant (SimplePrimitive Primitive.Bool, PrimitiveValue.Bool true)
        let body = Block []
        testParseWith "if ( true ) {}"
        <| should equal (Program [Body [(If (conditional, body))]])

    [<Test>]
    let ``When syntax for if statement is given with simple body, expect 'if' AST``() =
        let ident = SimpleIdentifier "x"
        let conditional = Constant (SimplePrimitive Primitive.Bool, PrimitiveValue.Bool false)
        let assignInBody = Body [Initialisation 
                                        ({identity = ident; isMutable = true; primitiveType = SimplePrimitive Primitive.Int}  // lvalue
                                        , Constant (SimplePrimitive Primitive.Int, PrimitiveValue.Int 23))
                                   ]
                                  // value
        let body = Block [ assignInBody ]
        let expected = (Program [Body [(If (conditional, body))]])
        debugTestParseWith "if ( false ) {var x:int := 23}"
        <| should equal expected 


    (* -------------------- Struct ---------------------- *)

    [<Test>]
    let ``When syntax for struct is given with 0 fields, expect struct AST``() =
        testParseWith "struct structName := {}"
        <| should equal (Program [Body [(Struct ("structName", []))]])

    [<Test>]
    let ``When syntax for struct is given with 1 field, expect struct AST``() =
        let fieldInBlock = ("field1", SimplePrimitive Primitive.Int)
        debugTestParseWith "struct structName := {field1:int;}"
        <| should equal (Program [Body [(Struct ("structName", [ fieldInBlock ]))]])

    [<Test>]
    let ``When syntax for struct is given with 2 fields, expect struct AST``() =
        let field1InBlock = ("field1", SimplePrimitive Primitive.Int)
        let field2InBlock = ("field2", SimplePrimitive Primitive.Real)
        debugTestParseWith "struct structName := {field1:int; field2:real;}"
        <| should equal (Program [Body [(Struct ("structName", [ field1InBlock; field2InBlock ]))]])



    (* ---------------------------- Send -------------------------- *)

    [<Test>]
    let ``When syntax for send is given, expect Send AST`` () =
        debugTestParseWith "send actorHandle recieverHandle msg"
        <| should equal (Program [Body [(Send ("actorHandle","recieverHandle", Identifier(SimpleIdentifier "msg",HasNoType)))]])

    (* --------------------------- Spawn --------------------------- *)

    [<Test>]
    let ``When syntax for spawn is given with immutable actor, expect Spawn AST`` () =
        let ident = SimpleIdentifier "actorHandle"
        let lhs = {identity = ident; isMutable = false; primitiveType = UserType "actorName"}
        let actorType = "actorName"
        let initMsg = Identifier (SimpleIdentifier "initMsg", HasNoType)
        debugTestParseWith "let actorHandle:actorName := spawn actorName initMsg"
        <| should equal (Program [Body [(Spawn (lhs,Some(actorType, Some initMsg)))]])

    [<Test>]
    let ``When syntax for spawn is given with mutable actor, expect Spawn AST`` () =
        let ident = SimpleIdentifier "actorHandle"
        let lhs = {identity = ident; isMutable = true; primitiveType = UserType "actorName"}
        let actorType = "actorName"
        let initMsg = Identifier (SimpleIdentifier "initMsg", HasNoType)
        debugTestParseWith "var actorHandle:actorName := spawn actorName initMsg"
        <| should equal (Program [Body [(Spawn (lhs, Some(actorType, Some initMsg)))]])

    (* --------------------------- Receive --------------------------- *)

    [<Test>]
    let ``When syntax for receive is given with empty body, expect Receive AST`` () =
        let msgType = SimplePrimitive Primitive.Int
        let body = Block []
        debugTestParseWith "receive msg:int := {}"
        <| should equal (Program [Body [(Receive ("msg", msgType, body))]])

    [<Test>]
    let ``When syntax for receive is given with msg of usertype and empty body, expect Receive AST`` () =
        let msgType = UserType "msgType"
        let body = Block []
        debugTestParseWith "receive msg:msgType := {}"
        <| should equal (Program [Body [(Receive ("msg", msgType, body))]])

    (* --------------------------- ForIn --------------------------- *)

    [<Test>]
    let ``When syntax for for-in-loop is given with empty body, expect ForIn AST`` () =
        let list = List ([0;1;2] |> List.map (fun n -> Constant (SimplePrimitive Primitive.Int, PrimitiveValue.Int n)), ListPrimitive (SimplePrimitive Int, 3))
        let body = Block []
        debugTestParseWith "for i in [0 .. 2] {}"
        <| should equal (Program [Body [(ForIn ("i", list, body))]])

    (* --------------------------- ListRange --------------------------- *)

    [<Test>]
    let ``When syntax for list from 0 to 3 is given, expect List AST`` () =
        let list = [0;1;2;3] |> List.map (fun n -> Constant (SimplePrimitive Primitive.Int, PrimitiveValue.Int n))
        debugTestParseWith "[0 .. 3]"
        <| should equal (Program [Body [(List (list,ListPrimitive (SimplePrimitive Int,4)))]])

    [<Test>]
    let ``When syntax for list from -5 to 2 is given, expect List AST`` () =
        let list = [-5;-4;-3;-2;-1;0;1;2] |> List.map (fun n -> Constant (SimplePrimitive Primitive.Int, PrimitiveValue.Int n))
        debugTestParseWith "[-5 .. 2]"
        <| should equal (Program [Body [(List (list,ListPrimitive (SimplePrimitive Int,8)))]])

    [<Test>]
    let ``When syntax for list from 5 to 1 is given, expect List AST`` () =
        let list = [5;4;3;2;1] |> List.map (fun n -> Constant (SimplePrimitive Primitive.Int, PrimitiveValue.Int n))
        debugTestParseWith "[5 .. 1]"
        <| should equal (Program [Body [(List (list,ListPrimitive (SimplePrimitive Int,5)))]])

    [<Test>]
    let ``When syntax for list from 5 to 1 to 5 is given, expect List AST`` () =
        let list = [5;4;3;2;1;2;3;4;5] |> List.map (fun n -> Constant (SimplePrimitive Primitive.Int, PrimitiveValue.Int n))
        debugTestParseWith "[5 .. 1,2 .. 5]"
        <| should equal (Program [Body [(List (list,ListPrimitive (SimplePrimitive Int,9)))]])


    [<Test>]
    let ``When syntax for list from 5 to 1, 5 to 1 is given, expect List AST`` () =
        let list = [5;4;3;2;1;5;4;3;2;1] |> List.map (fun n -> Constant (SimplePrimitive Primitive.Int, PrimitiveValue.Int n))
        debugTestParseWith "[5 .. 1,5 .. 1]"
        <| should equal (Program [Body [(List (list,ListPrimitive (SimplePrimitive Int,10)))]])

    (* --------------------------- Lists --------------------------- *)
    [<Test>]
    let ``When syntax for list consisting of [1,2,3], expect List AST`` () =
        let list = [1;2;3] |> List.map (fun n -> Constant (SimplePrimitive Primitive.Int, PrimitiveValue.Int n))
        debugTestParseWith "[1,2,3]"
        <| should equal (Program [Body [(List (list,ListPrimitive (SimplePrimitive Int,3)))]])

    (* --------------------------- Lists mixed --------------------------- *)

    [<Test>]
    let ``When syntax for list consisting of [1,2,3,1 .. 3], expect List AST`` () =
        let list = [1;2;3;1;2;3] |> List.map (fun n -> Constant (SimplePrimitive Primitive.Int, PrimitiveValue.Int n))
        debugTestParseWith "[1,2,3,1 .. 3]"
        <| should equal (Program [Body [(List (list,ListPrimitive (SimplePrimitive Int,6)))]])

    [<Test>]
    let ``When syntax for list consisting of [1,2,3,3 .. 1], expect List AST`` () =
        let list = [1;2;3;3;2;1] |> List.map (fun n -> Constant (SimplePrimitive Primitive.Int, PrimitiveValue.Int n))
        debugTestParseWith "[1,2,3,3 .. 1]"
        <| should equal (Program [Body [(List (list,ListPrimitive (SimplePrimitive Int,6)))]])

    (* --------------------------- Tuple --------------------------- *)
    [<Test>]
    let ``When syntax for tuple consisting of [1,"String",3.2], expect Tuple AST`` () =
        debugTestParseWith "(1,\"string\",3.2)"
        <| should equal (Program [Body [Tuple ([   Constant(SimplePrimitive Primitive.Int, PrimitiveValue.Int 1);
                                                Constant(ListPrimitive (SimplePrimitive Primitive.Char,6),
                                                    PrimitiveValue.List [
                                                                            PrimitiveValue.Char 's';
                                                                            PrimitiveValue.Char 't';
                                                                            PrimitiveValue.Char 'r';
                                                                            PrimitiveValue.Char 'i';
                                                                            PrimitiveValue.Char 'n';
                                                                            PrimitiveValue.Char 'g';
                                                                    ]);
                                                Constant(SimplePrimitive Primitive.Real, PrimitiveValue.Real 3.2)
                                           ],PrimitiveType.TupleType([SimplePrimitive Primitive.Int;SimplePrimitive Primitive.Char; SimplePrimitive Primitive.Real]))
                                    ]]
                                )

    (* --------------------------- Operation --------------------------- *)

    [<Test>]
    let ``When syntax for operation is given, expect Operation AST`` () =
        debugTestParseWith "2 + 3 + 4"
        <| should equal (Program [Body [
                                    BinOperation (
                                                    BinOperation (
                                                                    Constant (SimplePrimitive Primitive.Int, PrimitiveValue.Int 2),
                                                                    Plus,
                                                                    (Constant (SimplePrimitive Primitive.Int, PrimitiveValue.Int 3)) 
                                                                    ),
                                                    Plus,
                                                    (Constant (SimplePrimitive Primitive.Int, PrimitiveValue.Int 4))

                                               )
                                 ]])

    [<Test>]
    let ``When syntax for operation is given with modulo and equals, expect Operation AST`` () =
        debugTestParseWith "i % 20 = 0"
        <| should equal (Program [Body [
                                    BinOperation (
                                                    BinOperation (
                                                                    Identifier (SimpleIdentifier "i", HasNoType),
                                                                    Modulo,
                                                                    (Constant (SimplePrimitive Primitive.Int, PrimitiveValue.Int 20)) 
                                                                    ),
                                                    Equals,
                                                    (Constant (SimplePrimitive Primitive.Int, PrimitiveValue.Int 0))
                                               )
                                 ]])


    (* --------------------------- Functions --------------------------- *)
    [<Test>]
    let ``When syntax for function with zero arguments, expect Function AST`` () =
        let body = Block [ Body [ Constant (SimplePrimitive Primitive.Int, PrimitiveValue.Int 5) ] ]
        debugTestParseWith "let func1() : int := {5}"
        <| should equal (Program [Body [
                                    Function ("func1", [], SimplePrimitive Primitive.Int, body)
                                 ]])

    [<Test>]
    let ``When syntax for function with 1 argument, expect Function AST`` () =
        let body = Block [ Body [ Identifier (SimpleIdentifier "x", HasNoType) ] ]
        debugTestParseWith "let func1(x) : int -> int := {x}"
            <| should equal (Program [Body [
                                        Function ("func1", ["x"], ArrowPrimitive [SimplePrimitive Primitive.Int; SimplePrimitive Primitive.Int], body)
                                 ]])

    [<Test>]
    let ``When syntax for function with 2 arguments, expect Function AST`` () =
        let body = Block [ Body [ Identifier (SimpleIdentifier "x", HasNoType) ] ]
        debugTestParseWith "let func2(x, y) : int -> int -> int := {x}"
        <| should equal (Program [Body [
                                    Function (
                                        "func2", 
                                        ["x"; "y"], 
                                        ArrowPrimitive [SimplePrimitive Primitive.Int; SimplePrimitive Primitive.Int; SimplePrimitive Primitive.Int], 
                                        body
                                        )
                                 ]])

    (* --------------------------- Function Invocation --------------------------- *)
    [<Test>]
    let ``When syntax for function invocation with 0 parameters, expect Function invocation AST`` () =
        debugTestParseWith "f()"
        <| should equal (Program [Body [ Invocation ("f",[], ArrowPrimitive []) ]])


    [<Test>]
    let ``When syntax for function invocation with 1 userdefined parameter, expect Function invocation AST`` () =
        debugTestParseWith "f(x)"
        <| should equal (Program [Body [ Invocation ("f",["x"], HasNoType) ]])

    [<Test>]
    let ``When syntax for function invocation with 2 userdefined parameters, expect Function invocation AST`` () =
        debugTestParseWith "f(x,y)"
        <| should equal (Program [Body [ Invocation ("f",["x"; "y"], HasNoType) ]])

    [<Test>]
    let ``When syntax for function invocation with 3 userdefined parameters, expect Function invocation AST`` () =
        debugTestParseWith "f(x,y,z)"
        <| should equal (Program [Body [ Invocation ("f",["x"; "y"; "z"], HasNoType) ]])

    (* --------------------------- Struct Literal --------------------------- *)
    [<Test>]
    let ``When syntax for struct literal with 1 field, expect Struct literal AST`` () =
        let fieldName1 = "field1"
        let struct1 = StructLiteral (Program [], [(fieldName1, Constant (SimplePrimitive Primitive.Int, PrimitiveValue.Int 5))])
        debugTestParseWith "(field1 := 5)"
        <| should equal (Program [Body [ struct1  ]])

    [<Test>]
    let ``When syntax for struct literal with 2 fields, expect Struct literal AST`` () =
        let fieldName1 = "field1"
        let fieldName2 = "field2"
        let struct1 = StructLiteral (Program [],  [
                                        (fieldName1, Constant (SimplePrimitive Primitive.Int, PrimitiveValue.Int 5));
                                        (fieldName2, Constant (SimplePrimitive Primitive.Int, PrimitiveValue.Int 10));
                                    ])
        debugTestParseWith "(field1 := 5; field2 := 10)"
        <| should equal (Program [Body [ struct1  ]])

    (* --------------------------- String literal --------------------------- *)
    [<Test>]
    let ``When syntax for string literal, expect string literal AST`` () =
        debugTestParseWith "\"Tub\""
        <| should equal (Program [Body [ Constant (ListPrimitive (SimplePrimitive Primitive.Char, 3), PrimitiveValue.List [PrimitiveValue.Char 'T'; PrimitiveValue.Char 'u'; PrimitiveValue.Char 'b'])  ]])

    [<Test>]
    let ``When syntax for string literal with spaces, expect string literal AST`` () =
        debugTestParseWith "\"T ub\""
        <| should equal (Program [Body [ Constant (ListPrimitive (SimplePrimitive Primitive.Char, 4), PrimitiveValue.List [PrimitiveValue.Char 'T'; PrimitiveValue.Char ' '; PrimitiveValue.Char 'u'; PrimitiveValue.Char 'b'])  ]])


    (* --------------------------- NOT --------------------------- *)
    [<Test>]
    let ``When syntax for NOT true, expect UnaryOperation NOT AST`` () =
        debugTestParseWith "NOT true"
        <| should equal (Program [Body [ UnaryOperation (Not, Constant (SimplePrimitive Bool, PrimitiveValue.Bool true)) ]])

    (* --------------------------- Return --------------------------- *)
    [<Test>]
    let ``When syntax for return with value true, expect return AST`` () =
        debugTestParseWith "return true"
        <| should equal (Program [Body [ Return (Some (Constant (SimplePrimitive Bool, PrimitiveValue.Bool true))) ]])

    (* --------------------------- Die --------------------------- *)
    [<Test>]
    let ``When syntax for kill/me, expect kill + me AST`` () =
        debugTestParseWith "die;"
        <| should equal (Program [Body [ Die ]])

    (* --------------------------- List access --------------------------- *)
    [<Test>]
    let ``When syntax for list access, expect `` () =
        let ast = Identifier (IdentifierAccessor ("xs", Constant (SimplePrimitive Int, PrimitiveValue.Int 0)), HasNoType)
        debugTestParseWith "xs.[0];"
        <| should equal (Program [Body [ ast ]])