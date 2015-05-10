namespace vSprog

open System
open vSprog.CommonTypes
open vSprog.AST
open AnalysisUtils


module TypeChecker = 
    let checkTypesSymTable (symTable:SymbolTable) : Result<unit> =
        Success ()

    let rec checkTypesAST (root:AST) : Result<PrimitiveType> =
        match root with
        | Program stms | Block stms | Body stms ->
            stms 
            |> List.map (fun stm -> checkTypesAST stm)
            |> sumResults
        //| Reassignment ( varId, rhs) -> 

        //| Initialisation (lvalue, rhs) -> 
        //| Declaration ( name, ptype) -> 
        //| Constant (ptype,value) ->
        //| Actor (name, body) ->
        //| Struct ( name, field) -> 
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

        //| Send (actorName, msgName ) ->
        //| Spawn (lvalue, actorName, initMsg) -> 
        //| Receive (msgName, msgType, body) -> 
        //| ForIn(counterName, list, body) ->
        | While(condition, body) ->
            match checkTypesAST condition with
            | Failure errMsgs -> 
                Failure errMsgs
            | Success (SimplePrimitive Bool) -> 
                checkTypesAST body
            | Success illegalType ->
                Failure [sprintf "Conditional statement in while statement should be bool, found %A" illegalType]
        //| ListRange (content) ->
        //| BinOperation (lhs, op, rhs) ->
        //| UnaryOperation (op, rhs) ->
        //| Identifier name ->
        //| Function (funcName, arguments, types, body) ->
        //| StructLiteral fields ->
        //| Invocation (functionName, parameters) ->
        //| Return body ->
        //| Kill name ->
        //| Me ->


    let checkTypes (root:AST) (symTable:SymbolTable): Result<PrimitiveType> =
        checkTypesSymTable symTable >-> checkTypesAST root
