namespace vSprog

open System
open vSprog.CommonTypes
open vSprog.AST
open AnalysisUtils


module TypeChecker = 


    let Last (input:'a list) : 'a =
        List.rev input |>
        List.head



    let rec checkTypesAST (root:AST) : Result<PrimitiveType> =
        match root with
        | Program stms | Block stms | Body stms ->
            stms 
            |> List.map (fun stm -> checkTypesAST stm)
            |> sumResults
        | Reassignment ( varId, rhs) -> 
            Success HasNoType
        | Initialisation (lvalue, rhs) ->
            checkTypesAST rhs 
        | Constant (ptype,value) ->
            Success ptype
        | Actor (name, body) ->
            checkTypesAST body >->
            Success HasNoType
        | Struct ( name, field) ->
            Success HasNoType 
        | If (conditional, body) ->
            match checkTypesAST conditional with
            | Failure errMsgs -> 
                Failure errMsgs
            | Success (SimplePrimitive Bool) -> 
                checkTypesAST body
            | Success illegalType ->
                Failure [sprintf "Conditional statement in if statement should be bool, found %A" illegalType]

        | IfElse (conditional,trueBody,falseBody) ->
            match checkTypesAST conditional with
            | Failure errMsgs -> 
                Failure errMsgs
            | Success (SimplePrimitive Bool) -> 
                let trueRes = checkTypesAST trueBody
                let falseRes = checkTypesAST falseBody
                sumResults [trueRes; falseRes]
            | Success illegalType ->
                Failure [sprintf "Conditional statement in if-else statement should be bool, found %A" illegalType]
        | Send (actorName, msgName ) ->
            Success HasNoType
        | Spawn (lvalue, actorName, initMsg) ->
            Success HasNoType
        | Receive (msgName, msgType, body) -> 
            checkTypesAST body >->
            Success HasNoType
        | ForIn(counterName, list, body) ->
            checkTypesAST body >->
            match checkTypesAST list with
            | Success (ListPrimitive _) ->
                Success HasNoType
            | Success ptype ->
                Failure [sprintf "Expected (List), found %A" ptype ]
            | Failure errMsgs -> 
                Failure errMsgs
        | While(condition, body) ->
            match checkTypesAST condition with
            | Failure errMsgs -> 
                Failure errMsgs
            | Success (SimplePrimitive Bool) -> 
                checkTypesAST body
            | Success illegalType ->
                Failure [sprintf "Conditional statement in while statement should be bool, found %A" illegalType]
        | ListRange (content) ->
            match checkTypesAST content.Head with
            | Success head ->
                if List.forall(fun e -> checkTypesAST content.Head = checkTypesAST e) content then
                    Success (ListPrimitive head)
                else
                    Failure [sprintf "Lists are monomorphic, all element should of the same type as %A" head ]
            | Failure errMsgs -> 
                Failure errMsgs
        | BinOperation (lhs, op, rhs) ->
            match checkTypesAST lhs, op, checkTypesAST rhs with
            | Success lhsRes ,(Plus | Minus | Multiply | Divide | Modulo | Power), Success rhsRes ->
                match lhsRes, rhsRes with
                | SimplePrimitive Int, SimplePrimitive Int ->
                    Success (SimplePrimitive Int)
                | SimplePrimitive Real, SimplePrimitive Real ->
                    Success (SimplePrimitive Real)
                | _ ->
                    Failure [sprintf "Mismatch in imediate constituents types of binary operation(Plus | Minus | Multiply | Divide | Modulo | Power), found %A and %A" lhsRes rhsRes]
            | Success lhsRes , Root, Success rhsRes ->
                match lhsRes, rhsRes with
                | SimplePrimitive Int, SimplePrimitive Int ->
                    Success (SimplePrimitive Real)
                | SimplePrimitive Real, SimplePrimitive Real ->
                    Success (SimplePrimitive Real)
                | _ -> 
                    Failure [sprintf "Mismatch in imediate constituents types of binary operation(Root), found %A and %A" lhsRes rhsRes]
            | Success lhsRes, (GreaterThan | GreaterThanOrEq | LessThan | LessThanOrEq), Success rhsRes ->
                match lhsRes, rhsRes with
                | SimplePrimitive Int, SimplePrimitive Int ->
                    Success (SimplePrimitive Bool)
                | SimplePrimitive Real, SimplePrimitive Real ->
                    Success (SimplePrimitive Bool)
                | _ -> 
                    Failure [sprintf "Mismatch in imediate constituents types of binary operation(GreaterThan | GreaterThanOrEq | LessThan | LessThanOrEq), found %A and %A" lhsRes rhsRes]
            | Success lhsRes, (Equals | NotEquals), Success rhsRes ->
                match lhsRes, rhsRes with
                | lhsType, rhsType when lhsType = rhsType ->
                    Success (SimplePrimitive Bool)
                | _ -> 
                    Failure [sprintf "Mismatch in imediate constituents types of binary operation(Equals | NotEquals), found %A and %A" lhsRes rhsRes]
            | Success lhsRes, (And | Or | Xor | Nor | Nand), Success rhsRes ->
                match lhsRes, rhsRes with
                | SimplePrimitive Bool, SimplePrimitive Bool ->
                    Success (SimplePrimitive Bool)
                | _ -> 
                    Failure [sprintf "Mismatch in imediate constituents types of binary operation(And | Or | Xor | Nor | Nand), found %A and %A" lhsRes rhsRes]
            | (Failure errMsgsLhs as lhsFail), _, (Failure errMsgsRhs as rhsFail) -> 
                sumResults [lhsFail; rhsFail]
            | Failure errMsgs, _, _ -> 
                Failure errMsgs
            | _, _, Failure errMsgs -> 
                Failure errMsgs
                     
        | UnaryOperation (op, rhs) ->
            match op, checkTypesAST rhs with
            | Not, Success (SimplePrimitive Bool) ->
                Success (SimplePrimitive Bool)
            | Not, rhsRes ->
                Failure [sprintf "Cannot apply (Not) to anything else than Bools, found %A" rhsRes ]
        | Identifier name ->
            Success HasNoType
        | Function (funcName, arguments, types, body) ->
            match checkTypesAST body, types with
            | Success bodyType, ((ListPrimitive _ | SimplePrimitive _) as Arg) when bodyType = Arg ->
                Success HasNoType
            | Success bodyType, (ArrowPrimitive plist) ->
                Last plist |>
                fun e -> if bodyType = e then Success HasNoType 
                         else Failure [sprintf "Return value of function body does not match return of definiton"]
            | Success bodyType, _ ->
                Success HasNoType
            | Failure errMsgs, _ -> 
                Failure errMsgs
        | StructLiteral fields ->
            Success HasNoType
        | Invocation (functionName, parameters) ->
            Success HasNoType
        | Return body ->
            match body with
            | Some realBody -> checkTypesAST realBody
            | None -> Success HasNoType
        | Kill name ->
            Success HasNoType
        | Me ->
            Success HasNoType

    let checkTypesSymTable (symTable:SymbolTable) : Result<unit> =

        // for alle entries:
        // check om entry.symbol.primitiveType er lig typen af entry.value
        let results = List.map (fun entry -> 
                         match checkTypesAST entry.value with
                         | Success pType ->
                           if entry.symbol.primitiveType = pType then
                             Success ()
                           else 
                             Failure [sprintf "symbol %A expected to have type %A, but has type %A" entry.symbol entry.symbol.primitiveType pType]
                         | Failure err -> Failure err) symTable
        sumResults results
        
    let checkTypes (root:AST) (symTable:SymbolTable): Result<PrimitiveType> =
        checkTypesSymTable symTable >-> checkTypesAST root
