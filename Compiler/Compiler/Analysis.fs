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

    let argsFields = [
                      ("argv", ListPrimitive (ListPrimitive (SimplePrimitive Primitive.Char, 128), 128)); // TODO: vi bliver nødt til at hard code længden af lister
                      ("argc", SimplePrimitive Int)
                     ]
    let argsStruct = PrimitiveType.Struct ("args",argsFields)

    let getRealType (msgType:PrimitiveType) : State<PrimitiveType, Environment> =
        state {
            let! curState = getState
            let! curScope = getScope
            let realType = match msgType with
                           | UserType typeName ->
                             let sDecl = 
                                 curState.symbolList |>
                                   List.tryFind (fun e -> e.symbol.identity = SimpleIdentifier typeName && (e.statementType = Init || e.statementType = Def) && isInScope  e.scope curScope)
                             match sDecl with
                             | Some a -> a.symbol.primitiveType
                             | None -> HasNoType
                           | other -> other
            return realType
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
              let! newRhs = buildSymbolTable rhs

              match varId with
              | SimpleIdentifier _ ->
                let sDecl = 
                  curState.symbolList |>
                    List.tryFind (fun e -> e.symbol.identity = varId && e.statementType = Init && isInScope  e.scope curScope)
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
              | IdentifierAccessor (baseId, nextElem) ->
                let sDecl = 
                  curState.symbolList |>
                    List.tryFind (fun e -> e.symbol.identity = SimpleIdentifier baseId && e.statementType = Init && isInScope e.scope curScope)
                let newPType = match sDecl with
                               | Some a -> a.symbol.primitiveType
                               | None -> HasNoType

                let! realType = getRealType newPType

                let typeOfElem = match realType with
                                 | PrimitiveType.Struct (_, fields) ->
                                     let elemToFind = match nextElem with
                                                      | Identifier (SimpleIdentifier name, _) -> name
                                     let needle = fields
                                                  |> List.tryFind (fun (fieldName, _) -> elemToFind = fieldName)
                                     match needle with
                                     | Some (fieldName, fieldType) -> fieldType
                                     | None -> failwith (sprintf "Could not find %s in %A" elemToFind id)
                                 | ListPrimitive (pType, _) -> pType
                let entry = 
                    {
                      symbol = 
                        {
                        identity = varId
                        isMutable = 
                          match sDecl with
                          | Some (sEntry) -> sEntry.symbol.isMutable
                          | None -> false
                        primitiveType = typeOfElem
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

              let newnewRhs = match newRhs with
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
              let newLValPrimType = match newnewRhs with
                                    | List(_,prim) -> prim
                                    | StructLiteral ((Struct (name,decls)), _) -> PrimitiveType.Struct (name, decls)
                                    | other -> lvalue.primitiveType
              let! realType = getRealType newLValPrimType
              let newLValue = {lvalue with primitiveType = realType}

              let! curScopeAfter = getScope
              let entry =
                  {
                    symbol = newLValue
                    statementType = Init
                    scope = curScopeAfter
                    value = newnewRhs
                  }
              do! addEntry entry
              return Initialisation (newLValue, newnewRhs)
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
        | Send (actorHandle, actorToSendTo, msgName) -> 
          state {
              let! curState = getState
              let! curScope = getScope
              let sDecl = 
                curState.symbolList |>
                  List.tryFind (fun e -> e.symbol.identity = SimpleIdentifier actorHandle && e.statementType = Init && isInScope e.scope curScope)
              match sDecl with
              | Some entry ->
                let newActorToSendTo = match entry.symbol.primitiveType with
                                       | UserType actorName -> actorName
                                       | _ -> failwith "it should only be a simpleidentifier"
                return Send (actorHandle, newActorToSendTo, msgName)
              | None -> return failwith (sprintf "could not find actor initialisation of %A" actorHandle)
          }
        | Spawn (lvalue, rhs) -> 
          state {
              let! curScope = getScope
              let entry =
                      {
                        symbol = lvalue
                        statementType = Init
                        scope = curScope
                        value = Spawn (lvalue, rhs)
                      }
              do! addEntry entry
              match rhs with
              | Some (actorName, initMsg) ->

                  match initMsg with
                  | Some msg -> 
                      let! newInitMsg = buildSymbolTable msg
                      return Spawn (lvalue, Some (actorName, Some newInitMsg))
                  | None ->
                      return Spawn (lvalue, Some (actorName, initMsg))
              | None ->
                  return Spawn (lvalue, rhs)
          }
        | Receive (msgName, msgType, body) -> 
          state 
            {
              do! openScope

              let! curScope = getScope

              let! realMsgType = getRealType msgType

              let msgLvalue = {identity = SimpleIdentifier msgName; primitiveType = realMsgType; isMutable = false}
              let msgInitialisation = Initialisation (msgLvalue, Block [])

              let entry =
                  {
                    symbol = 
                      {
                        identity = SimpleIdentifier msgName
                        isMutable = false
                        primitiveType = realMsgType
                      }
                    statementType = Init
                    scope = curScope
                    value = Block []
                  }
              do! addEntry entry

              let! newBody =  buildSymbolTable body
              do! closeScope
              return Receive (msgName, realMsgType, newBody)
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

              //////////////////////////////////////
              match id with
              | SimpleIdentifier _ ->
                let sDecl = 
                  curState.symbolList |>
                    List.tryFind (fun e -> e.symbol.identity = id && e.statementType = Init && isInScope e.scope curScope)
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
              | IdentifierAccessor (baseId, nextElem) ->
                let sDecl = 
                  curState.symbolList |>
                    List.tryFind (fun e -> e.symbol.identity = SimpleIdentifier baseId && e.statementType = Init && isInScope e.scope curScope)
                let newPType = match sDecl with
                               | Some a -> a.symbol.primitiveType
                               | None -> HasNoType

                
                let! realType = getRealType newPType

                let typeOfElem = 
                   match nextElem with
                   | Identifier (SimpleIdentifier name, _) -> // struct indexing
                       match realType with
                       | PrimitiveType.Struct (_, fields) ->
                         let needle = fields
                                      |> List.tryFind (fun (fieldName, _) -> name = fieldName)
                         match needle with
                         | Some (fieldName, fieldType) -> fieldType
                         | None -> failwith (sprintf "Could not find %s in %A" name id)
                   | Constant (SimplePrimitive Int, _) -> // lust/tuple indexing
                       match realType with
                       | ListPrimitive (pType, _) -> pType

                let entry = 
                      {
                       symbol = 
                         {
                           identity = id
                           isMutable = match sDecl with
                                       | Some decl -> decl.symbol.isMutable
                                       | None -> false
                           primitiveType = typeOfElem
                         }
                       statementType = Use
                       scope = curScope
                       value = Identifier (id, typeOfElem)
                      }
                do! addEntry entry

                return Identifier (id, typeOfElem)
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
        | Tuple (elems, ptype) ->
          state 
            {
                let! fields = applyAll buildSymbolTable elems
                return Tuple (fields, ptype)
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
                        primitiveType = PrimitiveType.Struct (name, fields)
                      }
                    statementType = Def
                    scope = {outer = None; level = []} // outermost scope
                    value = Struct (name, fields)
                  }
          [entry]
        | _ -> []

    let analyse (ast:AST) : Result<AST> =
        let mainStructEntry = 
            {
             symbol = 
              {
                identity = SimpleIdentifier "args"
                isMutable = false
                primitiveType = argsStruct
              }
             statementType = Def
             scope = {outer = None; level = []} // outermost scope
             value = Struct ("args", argsFields)
            }
        let structEntries = mainStructEntry :: findAllStructs ast
        let (newAST, environment) = runState (buildSymbolTable ast) {symbolList = structEntries; errors = []; scope = {outer = None; level = []}; scopeCounter = 0; ast = Program []}
        checkReass environment.symbolList
        >>= checkUsedBeforeDecl
        >>= checkHiding
        >>= checkTypes newAST
        >-> Success newAST
