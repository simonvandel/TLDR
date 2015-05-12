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
            |> fun xs -> if xs.Length = 0 then
                           Success HasNoType
                         else
                           sumResults xs
        | Reassignment ( varId, rhs) -> 
            checkTypesAST rhs
            >-> Success HasNoType
        | Initialisation (lvalue, rhs) ->
            checkTypesAST rhs
            >-> Success HasNoType
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
                >-> Success HasNoType
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
                >-> Success HasNoType
            | Success illegalType ->
                Failure [sprintf "Conditional statement in if-else statement should be bool, found %A" illegalType]
        | Send (actorName, msgName ) ->
            Success HasNoType
        | Spawn (lvalue, actorName, initMsg) ->
            match initMsg with
            | Some msg -> 
              checkTypesAST msg
              >-> Success HasNoType
            | None ->
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
                >-> Success HasNoType
            | Success illegalType ->
                Failure [sprintf "Conditional statement in while statement:, found %A, expected bool" illegalType]
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
                    Failure [sprintf "Mismatch in immediate constituents types of binary operation %A, found %A and %A" op lhsRes rhsRes]
            | Success lhsRes , Root, Success rhsRes ->
                match lhsRes, rhsRes with
                | (SimplePrimitive Int | SimplePrimitive Real), (SimplePrimitive Int | SimplePrimitive Real) ->
                    Success (SimplePrimitive Real)
                | _ -> 
                    Failure [sprintf "Mismatch in immediate constituents types of binary operation %A, found %A and %A" op lhsRes rhsRes]
            | Success lhsRes, (GreaterThan | GreaterThanOrEq | LessThan | LessThanOrEq), Success rhsRes ->
                match lhsRes, rhsRes with
                | SimplePrimitive Int, SimplePrimitive Int ->
                    Success (SimplePrimitive Bool)
                | SimplePrimitive Real, SimplePrimitive Real ->
                    Success (SimplePrimitive Bool)
                | _ -> 
                    Failure [sprintf "Mismatch in immediate constituents types of binary operation %A, found %A and %A" op lhsRes rhsRes]
            | Success lhsRes, (Equals | NotEquals), Success rhsRes ->
                match lhsRes, rhsRes with
                | lhsType, rhsType when lhsType = rhsType ->
                    Success (SimplePrimitive Bool)
                | _ -> 
                    Failure [sprintf "Mismatch in immediate constituents types of binary operation %A, found %A and %A" op lhsRes rhsRes]
            | Success lhsRes, (And | Or | Xor | Nor | Nand), Success rhsRes ->
                match lhsRes, rhsRes with
                | SimplePrimitive Bool, SimplePrimitive Bool ->
                    Success (SimplePrimitive Bool)
                | _ -> 
                    Failure [sprintf "Mismatch in immediate constituents types of binary operation %A, found %A and %A, expected bool" op lhsRes rhsRes]
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
        | Identifier (name, pType) ->
            Success pType
        | Function (funcName, arguments, types, body) ->
            match checkTypesAST body, types with
            | Success bodyType, ((ListPrimitive _ | SimplePrimitive _) as Arg) when bodyType = Arg ->
                Success HasNoType
            | Success bodyType, (ArrowPrimitive plist) ->
                Last plist |>
                fun e -> if bodyType = e then Success HasNoType 
                         else Failure [sprintf "Return value of function body does not match return of definition. Found %A, expected %A" bodyType e]
            | Success bodyType, _ ->
                Success HasNoType
            | Failure errMsgs, _ -> 
                Failure errMsgs
        | StructLiteral fields ->
            let fieldBodies = fields |> List.map snd
            let fieldNames = fields |> List.map fst
            // TODO: hvordan tjekker vi det her??
            Success HasNoType
        | Invocation (functionName, parameters, functionType) ->
            match functionType with
            | SimplePrimitive pType when parameters.Length = 0 -> 
              Success functionType
            | ListPrimitive prim when parameters.Length = 0 ->
              Success functionType
            | ArrowPrimitive prims when parameters.Length = prims.Length - 1 ->
              Success (Last prims)
            | HasNoType -> Success HasNoType
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
                             Failure [sprintf "symbol %A expected to have type %A, but has type %A" entry.symbol.identity entry.symbol.primitiveType pType]
                         | Failure err -> Failure err) symTable
        if results.Length = 0 then
          Success ()
        else
          sumResults results
        
    let checkTypes (root:AST) (symTable:SymbolTable): Result<PrimitiveType> =
        (*checkTypesSymTable symTable >-> *)checkTypesAST root
