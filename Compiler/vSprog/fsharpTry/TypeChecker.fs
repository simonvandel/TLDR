namespace vSprog

open System
open vSprog.CommonTypes
open vSprog.AST
open AnalysisUtils


module TypeChecker = 
    let rec checkTypes (root:AST) (symTable:SymbolTable): Result<PrimitiveType> =
        match root with
        | Program stms | Block stms | Body stms ->

        //| Assignment (mutability, varId, value) ->
        //| Reassignment ( varId, rhs) -> 
        //| Initialisation (lvalue, rhs) -> 
        //| Declaration ( name, ptype) -> 
        //| Constant (ptype,value) ->
        //| Actor (name, body) ->
        //| Struct ( name, field) -> 
        | If (conditional, body) ->
            match checkTypes conditional symTable with
            | Failure errMsgs -> 
                Failure errMsgs
            | Success (SimplePrimitive Bool) -> 
                checkTypes body symTable
            | Success illegalType ->
                Failure [sprintf "Conditional statement in if statement should be bool, found %A" illegalType]

        | IfElse (conditional,trueBody,falseBody) ->
            match checkTypes conditional symTable with
            | Failure errMsgs -> 
                Failure errMsgs
            | Success (SimplePrimitive Bool) -> 
                let trueRes = checkTypes trueBody symTable
                let falseRes = checkTypes falseBody symTable
                sumResults [trueRes; falseRes]
            | Success illegalType ->
                Failure [sprintf "Conditional statement in if-else statement should be bool, found %A" illegalType]

        //| Send (actorName, msgName ) ->
        //| Spawn (lvalue, actorName, initMsg) -> 
        //| Receive (msgName, msgType, body) -> 
        //| ForIn(counterName, list, body) ->
        | While(condition, body) ->
            match checkTypes condition symTable with
            | Failure errMsgs -> 
                Failure errMsgs
            | Success (SimplePrimitive Bool) -> 
                checkTypes body symTable
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

      //Success (SimplePrimitive Primitive.Int)
