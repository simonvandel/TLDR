namespace vSprog

open Hime.CentralDogma;
open Hime.Redist;
open vSprog.CommonTypes
open vSprog.AST
open AnalysisUtils
open vSprog.TypeChecker

module Analysis =
    // applies a state workflow to all elements in list
    let rec applyAll (p:('a ->State<'a,'b>)) (list:'a list) : State<'a list, 'b> =
        state {
            match list with
            | [] -> return []
            | x::xs ->
                let! x1 = p x
                let! xs1 = applyAll p xs
                return x1 :: xs1
              }

    let rec buildSymbolTable (root:AST) :  State<AST, Environment> =
        match root with
        | Program stms ->
          state 
            {
              let! newBodies = applyAll buildSymbolTable stms
              return (Program newBodies)
            }
        | Block stms ->
          state 
            {
              let! newBodies = applyAll buildSymbolTable stms
              return (Block newBodies)
            }
        | Body stms -> 
          state 
            {
              do! openScope
              let! newBodies = applyAll buildSymbolTable stms
              do! closeScope
              return (Body newBodies)
            }
        | Reassignment (varId, rhs) as reass ->
          state 
            {
              let! curScope = getScope
              let! curState = getState
              let idToCheck = match varId with
                              | SimpleIdentifier _ as simpleId -> simpleId
                              | IdentifierAccessor (startId, nextElem) -> SimpleIdentifier startId
              let sDecl = 
                curState.symbolList |>
                  List.tryFind (fun e -> e.symbol.identity = idToCheck && e.statementType = Init && isInScope  e.scope curScope)
              let! newRhs = buildSymbolTable rhs
              let entry = 
                  {
                    symbol = 
                      {
                      identity = varId
                      isMutable = 
                        match sDecl with
                        | Some (sEntry) -> sEntry.symbol.isMutable
                        | None -> false
                      primitiveType = 
                        match sDecl with
                        | Some (sEntry) -> sEntry.symbol.primitiveType
                        | None -> HasNoType
                      }
                    statementType = Reass
                    scope = curScope
                    value = newRhs
                  }
              do! addEntry entry

              return Reassignment (varId, newRhs)
            }
        | Initialisation (lvalue, rhs) as init ->
          state
            {
              let! curState = getState
              let! curScope = getScope
              let! newRhs = buildSymbolTable rhs
              let newLValPrimType = match newRhs with
                                    | List(_,prim) -> prim
                                    | other -> lvalue.primitiveType
              let newLValue = {lvalue with primitiveType = newLValPrimType}
              let newRhs = match newRhs with
                           | StructLiteral (structToInit, fieldNamesAndValues) ->
                             let structToInitName = match lvalue.primitiveType with
                                                    | UserType name -> name
                             let findStructDef (e:SymbolTableEntry) : bool = // find the entry where the name is the struct we wish to find
                                 match e.symbol.identity with
                                 | SimpleIdentifier structName -> structName = structToInitName
                                 | _ -> false
                             let entry = curState.symbolList
                                                   |> List.tryFind (fun e -> findStructDef e && e.statementType = Def && isInScope e.scope curScope)
                             let newStructToInit = match entry with
                                                   | Some a -> a.value
                                                   | None -> failwith "could not find struct declaration"
                             StructLiteral (newStructToInit, fieldNamesAndValues)
                           | other -> other
              
              let! curScopeAfter = getScope
              let entry =
                  {
                    symbol = newLValue
                    statementType = Init
                    scope = curScopeAfter
                    value = newRhs
                  }
              do! addEntry entry
              return Initialisation (newLValue, newRhs)
            }
      
        | Actor (name, body) -> 
          state
            {
              let! curScope = getScope
              let! newBody = buildSymbolTable body
              let newAST = Actor (name, newBody)
              let entry =
                  {
                    symbol = 
                      {
                        identity = SimpleIdentifier name
                        isMutable = false
                        primitiveType = HasNoType
                      }
                    statementType = Def
                    scope = curScope
                    value = newAST
                  }
              do! addEntry entry

              return newAST
            }
        | Struct (name, fields) ->
          state
            {
              return Struct (name, fields)
            }
        | If (condition, body) -> 
          state 
            {
              do! openScope
              let! newCondition = buildSymbolTable condition
              let! newBody = buildSymbolTable body
              do! closeScope
              return If (newCondition, newBody)
            }
        | Send (actorName, msgName) -> 
          state {
              return Send (actorName, msgName)
          }
        | Spawn (lvalue, RHS) -> 
          state {
              match RHS with
              | Some (actorName, initMsg) ->
                  match initMsg with
                  | Some msg -> 
                      let! newInitMsg = buildSymbolTable msg
                      return Spawn (lvalue, Some (actorName, Some newInitMsg))
                  | None ->
                      return Spawn (lvalue, Some (actorName, initMsg))
              | None ->
                  return Spawn (lvalue, RHS)
          }
        | Receive (msgName, msgType, body) -> 
          state 
            {
              do! openScope
              let! newBody =  buildSymbolTable body
              do! closeScope
              return Receive (msgName, msgType, newBody)
            }
        | ForIn (counterName, list, body) -> 
          state 
            {
              do! openScope
              let! curScope = getScope
              let! newList = buildSymbolTable list
              let entry =
                  {
                    symbol = 
                      {
                        identity = SimpleIdentifier counterName
                        isMutable = false
                        primitiveType = match newList with
                                        | List (content, pType) -> pType
                                        | Identifier (id, pType) -> pType
                                        | _ -> HasNoType
                      }
                    statementType = Init
                    scope = curScope
                    value = Block []
                  }
              do! addEntry entry
              let! newBody = buildSymbolTable body
              do! closeScope
              return ForIn (counterName, newList, newBody)
            }
        | List (content, pType) -> 
            state {
                let! newContent = applyAll buildSymbolTable content
                return List (newContent, pType)
            }
        | BinOperation (lhs, op, rhs) -> 
          state
            {
              let! newLhs = buildSymbolTable lhs
              let! newRhs = buildSymbolTable rhs
              return BinOperation (newLhs, op, newRhs)
            }
        | Identifier (id, pType) -> 
          state
            {
              let! curScope = getScope
              let! curState = getState
              let idToCheck = match id with
                              | SimpleIdentifier _ as simpleId -> simpleId
                              | IdentifierAccessor (startId, nextElem) -> SimpleIdentifier startId
              let sDecl = 
                curState.symbolList |>
                  List.tryFind (fun e -> e.symbol.identity = idToCheck && e.statementType = Init && isInScope e.scope curScope)
              let newPType = match sDecl with
                             | Some decl -> decl.symbol.primitiveType
                             | None -> HasNoType
              let entry = 
                {
                  symbol = 
                    {
                      identity = id
                      isMutable = match sDecl with
                                  | Some decl -> decl.symbol.isMutable
                                  | None -> false
                      primitiveType = newPType
                    }
                  statementType = Use
                  scope = curScope
                  value = Identifier (id, newPType)
                }
              do! addEntry entry
              return Identifier (id, newPType)
            }
        | Function (funcName, arguments, types, body) -> 
          state 
            {
              let! outside = getScope
              do! openScope
              let! inside = getScope
              // add arguments as symbols
              let argsWithTypes = match types with
                                  | SimplePrimitive _ | ListPrimitive _ | HasNoType -> [] // no arguments to add
                                  | ArrowPrimitive prims ->
                                    Seq.zip arguments prims
                                    |> List.ofSeq

              let argumentEntries = argsWithTypes
                                    |> List.map (fun (argName, argType) -> 
                                         {
                                          symbol = 
                                            {
                                              identity = SimpleIdentifier argName
                                              isMutable = false
                                              primitiveType = argType
                                            }
                                          statementType = Init
                                          scope = inside
                                          value = Identifier (SimpleIdentifier argName, argType)
                                        })

              do! forAll addEntry argumentEntries

              let! newBody = buildSymbolTable body
              let entry =
                  {
                    symbol = 
                      {
                        identity = SimpleIdentifier funcName
                        isMutable = false
                        primitiveType = types  //SimplePrimitive (Primitive.Function types)
                      }
                    statementType = Def
                    scope = outside
                    value = Function (funcName, arguments, types, newBody)
                  }
              do! addEntry entry

              do! closeScope
              return Function (funcName, arguments, types, newBody)
            }
        | StructLiteral (structToInit, fieldNamesAndVals) -> 
          state
            {
              let fields = fieldNamesAndVals |> List.map (fun e -> snd e)
              let fieldNames = fieldNamesAndVals |> List.map (fun e -> fst e)
              let! newFields = applyAll buildSymbolTable fields
              return StructLiteral (structToInit, List.zip fieldNames newFields)
            }
        | Invocation (functionName, parameters, functionType) -> 
          state {
              let! curScope = getScope
              let! curState = getState
              let sDecl = 
                curState.symbolList |>
                  List.tryFind (fun e -> e.symbol.identity = SimpleIdentifier functionName && e.statementType = Def && isInScope e.scope curScope)
              

              let newPType = match sDecl with
                             | Some decl -> decl.symbol.primitiveType
                             | None -> HasNoType
              return Invocation (functionName, parameters, newPType)
          }
        | UnaryOperation (op, rhs) -> 
          state
            {
              let! newRhs = buildSymbolTable rhs
              return UnaryOperation (op, rhs)
            }
        | While (cond, body) ->
          state 
            {
              do! openScope
              let! newCondition = buildSymbolTable cond
              let! newBody = buildSymbolTable body
              do! closeScope
              return While (newCondition, newBody)
            }
        | Die -> 
            state {
                return Die
            }
        | Return (arg) -> 
          state
            {
              match arg with
              | Some someArg -> 
                let! newArg = buildSymbolTable someArg
                return Return (Some newArg)
              | None -> 
                return Return None
            }
        | IfElse(cond, tBody, fBody) ->
          state
            {
              do! openScope
              let! newCondition = buildSymbolTable cond
              do! closeScope

              do! openScope
              let! newTBody = buildSymbolTable tBody
              do! closeScope

              do! openScope
              let! newFBody = buildSymbolTable fBody
              do! closeScope

              return IfElse(newCondition, newTBody, newFBody)
            }
        | Constant (ptype, value) -> 
          state {
              return Constant (ptype, value)
          }

    let checkHiding (symbolTable:SymbolTable) : Result<SymbolTable> =
        // find alle dupliketter
        // for hver dupliket a, se om dupliketter b, har b.scope.outer = a.scope eller b.scope = a.scope
        // første entry af a: er nogen i samme scope af dem under mig i listen?
        let mutable res:Result<SymbolTable> list = []
        let NoReass = symbolTable |> List.filter (fun e -> e.statementType <> Reass && e.statementType <> Use)
        for i in NoReass do
          let seq = List.filter (fun e -> e <> i && isVisible i e && i.symbol.identity = e.symbol.identity && i <> e) NoReass
          if seq.Length > 0 then 
            res <- Failure [sprintf "Element %A hides %A" i.symbol.identity seq] :: res

        match res with
        | [] -> Success symbolTable
        | xs -> sumResults xs

    let checkUsedBeforeDecl (symTable:SymbolTable) : Result<SymbolTable> =
      let res = symTable |>
                    List.filter (fun e -> e.statementType <> Def && e.symbol.primitiveType = PrimitiveType.HasNoType ) |>
                      List.map (fun e -> Failure [sprintf "Symbol \"%A\" is used before its declaration." e.symbol.identity])
      match res with
      | [] -> Success symTable
      | xs -> sumResults xs

    let checkReass symTable = 
      //printfn "%A" symTable
      let res = symTable |> 
                  List.filter (fun entry -> entry.statementType = Reass) |>
                    List.filter (fun entry -> entry.symbol.isMutable = false) |>
                      List.map (fun e -> Failure [sprintf "Invalid reassignment, %A is not mutable" e.symbol.identity])
      match res with
      | [] -> Success symTable
      | xs -> sumResults xs

    let flatten listlist = [for lst in listlist do yield! lst]
    let rec findAllStructs (ast:AST) : SymbolTable =
        match ast with
        | Program stms | Body stms | Block stms ->
          stms
          |> List.map findAllStructs
          |> flatten
        | Struct (name, fields) ->
          let entry =
                  {
                    symbol = 
                      {
                        identity = SimpleIdentifier name
                        isMutable = false
                        primitiveType = SimplePrimitive (Primitive.Struct (name, fields))
                      }
                    statementType = Def
                    scope = {outer = None; level = []} // outermost scope
                    value = Struct (name, fields)
                  }
          [entry]
        | _ -> []

    let analyse (ast:AST) : Result<AST> =
        let structEntries = findAllStructs ast
        let (newAST, environment) = runState (buildSymbolTable ast) {symbolList = structEntries; errors = []; scope = {outer = None; level = []}; scopeCounter = 0; ast = Program []}
        checkReass environment.symbolList
        >>= checkUsedBeforeDecl
        >>= checkHiding
        >>= checkTypes newAST
        >-> Success newAST
