namespace vSprog

open Hime.CentralDogma;
open Hime.Redist;
open vSprog.CommonTypes
open vSprog.AST

module Analysis =

    type StatementType =
        | Init // var x:int := 5
        | Decl // x: int
        | Ass // var x := 5
        | Reass // x := 5
        | Def // Actor A := {} | Struct B := {}

    type Scope = {
        outer:Scope option //TODO: fjern outer, da det bare kan ses ud fra level
        level:int list
    }

    type SymbolTableEntry = {
        symbol: LValue
        statementType:StatementType
        scope:Scope
        value:AST
    }

    (*type SymbolTableEntry = {
        error:bool
        symbol:LValue
        statementType:StatementType
        scope:Scope
    }*)

    type SymbolTable = SymbolTableEntry list //Map<Scope, Map<string, PrimitiveType>>  //

    type Environment =  {
        symbolList: SymbolTable
        errors: string list
        scope: Scope
        scopeCounter: int
    }

    // applies a state workflow to all elements in list
    let rec forAll (p:('a ->State<unit,'b>)) (list:'a list) : State<unit, 'b> =
        state {
            match list with
            | [] -> return ()
            | x::xs ->
                do! p x
                do! forAll p xs
              }

    let openScope : State<unit, Environment> =
        state {
                let! state = getState
                let newScopeCtr = state.scopeCounter + 1
                let newState = {state with scope = {outer = Some state.scope; level = newScopeCtr :: state.scope.level}
                                           scopeCounter = newScopeCtr
                                }
                do! putState newState
              }

    let closeScope : State<unit, Environment> =
        state {
                let! state = getState
                let newState = {state with scope = {outer = Option.bind (fun sc -> sc.outer) state.scope.outer; level = state.scope.level.Tail}
                                           //scopeCounter = 0
                                }
                do! putState newState
              }

    let addEntry (entry:SymbolTableEntry) : State<unit, Environment> =
        state {
                let! state = getState
                let newState = {state with symbolList = state.symbolList @ [entry]}
                do! putState newState
              }

    let getScope : State<Scope, Environment> =
        state {
            let! stat = getState
            return stat.scope
        }

    let SameState = 
      state 
        {
          let! curState = getState
          do! putState curState
        }
(*
    let rec typecheck (root:AST) : Result<PrimitiveType> =
        match root with
        | Program stms ->
            stms
            |> List.map typecheck
            |> addResults
        | Body stms ->
            stms
            |> List.map typecheck
            |> addResults
        | Block stms ->
            stms
            |> List.map typecheck
            |> addResults
        | Initialisation (lvalue, value) ->
            typecheck value
            >>= fun primType -> if lvalue.primitiveType = primType then
                                    Success primType
                                else Failure [sprintf "Initialisation does not typecheck. Expected type %A, found type %A" lvalue.primitiveType primType]
        | Assignment (mutability, varId, rhs) ->
            printfn "Assignment bliver ikke typechecked ordentligt pt!!!"
            typecheck rhs
        | Constant (primType, _) ->
            Success primType
        | If (condition, body) ->
            match typecheck condition with
            | Success (SimplePrimitive Primitive.Bool) ->
                typecheck body
            | Success otherThanBool ->
                Failure [sprintf "Expected condition of type bool, found condition of type %A" otherThanBool]
            | Failure msg -> Failure msg
        | Actor (_, body) ->
            typecheck body
        | Receive (_, msgType, body) ->
            typecheck body
        | StructLiteral fieldNamesAndValues ->
            for i in fieldNamesAndValues do
              match typecheck (snd i) with
              | Success otherThanBool ->
                ()
              | Failure errs -> failwith "Struct-literal failed to typecheck with these errors: %A" errs
            //printfn "StructLiteral bliver ikke typechecked ordentligt pt!!!"
            Success (HasNoType) // FIXME: IKKE IMPLEMENTERET
        | ForIn (counterName, list, body) ->
            [list; body]
            |> List.map typecheck
            |> addResults
        | ListRange values ->
            // all elements in list has to have same type
            let firstElemType = match typecheck values.Head with
                                | Success primType -> primType
                                | Failure errs -> failwith "First element of list does not typecheck, which should never happen?"

            values
            |> List.forall (fun elem -> match typecheck elem with
                                        | Success primType -> firstElemType = primType
                                        | Failure _ -> false)
            |> fun res -> match res with
                          | true -> Success firstElemType
                          | false -> Failure [sprintf "Not all elements in list have type %A" firstElemType]

        | Spawn (lvalue, actorName, initMsg) ->
            match lvalue.primitiveType, actorName with
            | UserType lvalType, rvalType ->
                if lvalType = rvalType then
                    Success HasNoType
                else
                    Failure [sprintf "Spawn does not typecheck. Expected actorType %A, but found %A" lvalType rvalType]
            | _, _ ->
                Failure [sprintf "Spawn does not typecheck. Types specified are not identifiers."]
        | Reassignment (varId, rhs) ->
            printfn "Reassignment bliver ikke typechecked ordentligt pt!!!"
            Success HasNoType // FIXME: IKKE IMPLEMENTERET
        | Struct (name, fields) ->
            Success HasNoType
        | Send (actorName, msgName) ->
            Success HasNoType
        | Operation (lhs, op, rhs) ->
            match typecheck lhs, op, typecheck rhs with
            (* Int|Real and Int|Real Equals *)
            | Success (SimplePrimitive Primitive.Int | SimplePrimitive Primitive.Real) , Equals, Success (SimplePrimitive Primitive.Real | SimplePrimitive Primitive.Int) ->
                Success (SimplePrimitive Primitive.Bool)

            (* Int|Real and Int|Real Plus|Minus|Modulo|Multiply *)
            | Success (SimplePrimitive Primitive.Int | SimplePrimitive Primitive.Real) , (Plus|Minus|Modulo|Multiply), Success (SimplePrimitive Primitive.Real | SimplePrimitive Primitive.Int) ->
                Success (SimplePrimitive Primitive.Real)

            (* Default for cases allowed Plus|Minus|Modulo|Multiply *)
            | Success (lhs), (Plus|Minus|Modulo|Multiply), Success (rhs) when lhs = rhs ->
                Success lhs

            (* Default for cases allowed Equals *)
            | Success (lhs), Equals, Success (rhs) when lhs = rhs ->
                Success (SimplePrimitive Primitive.Bool)

            (* Default for cases not allowed *)
            | Success (lhs), (Plus|Minus|Modulo|Equals|Multiply as op), Success (rhs) ->
                Failure [sprintf "%A of %A with %A not allowed." op lhs rhs]

            | Failure errs, (Plus|Minus|Modulo|Equals|Multiply as op), Failure errs2 ->
                Failure (errs @ errs2)

            | Success _, (Plus|Minus|Modulo|Equals|Multiply as op), Failure errs ->
                Failure (errs)

            | Failure errs, (Plus|Minus|Modulo|Equals|Multiply as op), Success _ ->
                Failure (errs)

        | Function (funcName, arguments, types, body) ->
            typecheck body
        | Invocation (functionName, parameters) ->
            printfn "Invocation bliver ikke typechecked ordentligt pt!!!"
            Success HasNoType // FIXME: IKKE IMPLEMENTERET
        | Identifier (id) ->
            printfn "Identifier bliver ikke typechecked ordentligt pt!!!"
            Success HasNoType // FIXME: IKKE IMPLEMENTERET
        | other -> Failure [sprintf "%A not typechecked" other]

    let rec buildSymbolTable (root:AST) : State<unit, Environment> =
        match root with
        | Program stms ->
            state {
                do! forAll buildSymbolTable stms
            }
        | Block stms ->
            state {
                do! openScope
                do! forAll buildSymbolTable stms
                do! closeScope
            }
        | Assignment (mutability, varId, body) ->
            state {
                let! scope = getScope
                let lValue = {identity = SimpleIdentifier varId; isMutable = mutability; primitiveType = match typecheck body with
                                                                                                         | Success prim -> prim
                                                                                                         | Failure errs -> failwith "nej"}
                let entry = {
                             error = false;
                             symbol = lValue;
                             statementType = Ass;
                             scope = scope}
                do! addEntry entry
                 }
        | Actor (name, body) ->
            state {

                    let! scope = getScope
                    let lValue = {identity = SimpleIdentifier name; isMutable = false; primitiveType = SimplePrimitive (Primitive.Actor name)}
                    let entry = {
                             error = false;
                             symbol = lValue;
                             statementType = Ass;
                             scope = scope}
                    do! addEntry entry
                    do! buildSymbolTable body
                    do! openScope
                    do! closeScope
                  }
        | Body stms ->
            state {
                do! openScope
                do! forAll buildSymbolTable stms
                do! closeScope
            }
        | Constant (type', value) ->
            state {
                let! s = getState
                do! putState s
            }
        | ForIn (counterName, list, body) ->
            state {
                do! openScope
                let! scope = getScope
                let elemListType = match list with
                                   | ListRange (Constant (type',_) :: _) -> type'
                                   | err -> failwith (sprintf "Expected it to only be a list, but found: %A" err)
                let entry = {
                             error = false;
                             symbol = {identity = SimpleIdentifier counterName; isMutable = false; primitiveType = elemListType;}
                             statementType = Ass; //FIXME: is this the correct statementType?
                             scope = scope}
                do! addEntry entry
                do! buildSymbolTable body
                do! closeScope
            }
        | Function (funcName, arguments, types, body) ->
            state {
                    let! outsideScope = getScope
                    let entry = {
                             error = false;
                             symbol = {identity = SimpleIdentifier funcName; isMutable = false; primitiveType = types;}
                             statementType = Decl;
                             scope = outsideScope}
                    do! addEntry entry
                    do! openScope
                    let! insideScope = getScope
                    let rec typesToList (inputType:PrimitiveType) : PrimitiveType list =
                        match inputType with
                        | SimplePrimitive _ as prim -> [prim]
                        | ListPrimitive listType -> typesToList listType
                        | ArrowPrimitive xs -> xs
                        | UserType _ as prim -> [prim]
                        | HasNoType as prim -> [prim]

                    let typesAsList = typesToList types
                    let argumentEntries = Seq.zip arguments typesAsList
                                          |> List.ofSeq
                                          |> List.map (fun (argument, type') ->
                                              {
                                                error = false;
                                                symbol = {identity = SimpleIdentifier argument; isMutable = false; primitiveType = type';}
                                                statementType = Decl;
                                                scope = insideScope
                                              })
                    do! forAll addEntry argumentEntries
                    do! closeScope
                }
        | Identifier id ->
            state {
                let! s = getState
                do! putState s
            }
        | If (conditional, body) ->
            state {
                    let! s = getState
                    do! putState s
                  }
        | Invocation (functionName, parameters) ->
            state {
                    let! s = getState
                    do! putState s
                  }
        | ListRange content ->
            state {
                    let! s = getState
                    do! putState s
                  }
        | Operation (lhs, op, rhs) ->
            state {
                    let! s = getState
                    do! putState s
                  }
        | Reassignment (varId, rhs) ->
            state {
                let! scope = getScope
                let lValue = {identity = varId; isMutable = true; primitiveType = StillUnknown rhs}
                let entry = {
                             error = false;
                             symbol = lValue;
                             statementType = Ass;
                             scope = scope}
                do! addEntry entry
                do! openScope
                do! buildSymbolTable rhs
                do! closeScope
                 }
        | Receive (msgName, msgType, body) ->
            state {
                    do! openScope
                    let! bodyScope = getScope
                    let lValue = {identity = SimpleIdentifier msgName; isMutable = false; primitiveType = msgType}
                    let entry = {
                             error = false;
                             symbol = lValue;
                             statementType = Decl;
                             scope = bodyScope}
                    do! addEntry entry
                    do! buildSymbolTable body
                    do! closeScope
                  }
        | Send (actorName, msgName) ->
            state {
                    let! s = getState
                    do! putState s
                  }
        | Spawn (lvalue, actorName, initMsg) ->
            state {
                    let! curScope = getScope
                    let entry = {
                             error = false;
                             symbol = lvalue;
                             statementType = Init;
                             scope = curScope
                                }
                    do! addEntry entry
                  }
        | Struct (name, fields) ->
            state {
                    let! s = getState
                    do! putState s
                  }
        | Initialisation (lvalue, rhs) ->
            state {
                    let! curScope = getScope
                    let entry = {
                             error = false;
                             symbol = lvalue;
                             statementType = Init;
                             scope = curScope
                                }
                    do! addEntry entry
                    do! openScope
                    do! buildSymbolTable rhs
                    do! closeScope
                  }
        | StructLiteral (fields) ->
            state {
                    let! s = getState
                    do! putState s
                  }
                  *)


    let rec buildSymbolTable (root:AST) :  State<unit, Environment> =
        match root with
        | Program stms | Block stms | Body stms-> 
          state 
            {
              do! forAll buildSymbolTable stms
            }
        | Assignment (mutability, varId, rhs) as ass-> 
          state 
            {
              let! curScope = getScope
              let! curState = getState
              let entry = 
                {
                  symbol = 
                    {
                      identity = SimpleIdentifier varId
                      isMutable = mutability
                      primitiveType = 
                        curState.symbolList |>
                          List.tryFind (fun e -> e.symbol.identity = SimpleIdentifier varId && (e.statementType = Decl || e.statementType = Init)) |>
                          (fun e -> match e with 
                                      | Some (sEntry) -> sEntry.symbol.primitiveType
                                      | None -> StillUnknown ass
                          )
                    }
                  statementType = Ass
                  scope = curScope
                  value = rhs
                }
              do! addEntry entry
            }
        | Reassignment (varId, rhs) as reass ->
          state 
            {
              let! curScope = getScope
              let! curState = getState
              let sDecl = 
                curState.symbolList |>
                  List.tryFind (fun e -> e.symbol.identity = varId && (e.statementType = Decl || e.statementType = Init))
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
                        | None -> StillUnknown reass
                      }
                    statementType = Reass
                    scope = curScope
                    value = rhs
                  }
              do! addEntry entry
            }
        | Initialisation (lvalue, rhs) as init ->
          state
            {
              let! curScope = getScope
              let entry =
                  {
                    symbol = lvalue
                    statementType = Init
                    scope = curScope
                    value = rhs
                  }
              do! addEntry entry
            }
        | Declaration (name, ptype) as decl ->
          state
            {
              let! curScope = getScope
              let entry =
                  {
                    symbol =
                      {
                        identity = SimpleIdentifier name
                        isMutable = false
                        primitiveType = ptype
                      }
                    statementType = Decl
                    scope = curScope
                    value = decl
                  }
              do! addEntry entry
            }
        | Constant (ptype, value)-> SameState
        | Actor (name, body) -> 
          state
            {
              let! curScope = getScope
              let entry =
                  {
                    symbol = 
                      {
                        identity = SimpleIdentifier name
                        isMutable = false
                        primitiveType = SimplePrimitive (Primitive.Actor name)
                      }
                    statementType = Def
                    scope = curScope
                    value = body
                  }
              do! addEntry entry
            }
        | Struct (name, fields) ->
          state
            {
              let! curScope = getScope
              let entry =
                  {
                    symbol = 
                      {
                        identity = SimpleIdentifier name
                        isMutable = false
                        primitiveType = SimplePrimitive (Primitive.Struct (name, fields))
                      }
                    statementType = Def
                    scope = curScope
                    value = Struct (name, fields)
                  }
              do! addEntry entry
            }
        | If (condition, body) -> SameState
        | Send (actorName, msgName) -> SameState
        | Spawn (lvalue, actorName, initMsg) -> SameState
        | Receive (msgName, msgType, body) -> SameState
        | ForIn (counterName, list, body) -> SameState
        | ListRange content -> SameState
        | Operation (lhs, op, rhs) -> SameState
        | Identifier id -> SameState
        | Function (funcName, arguments, types, body) -> SameState
        | StructLiteral fieldNamesAndVals -> SameState
        | Invocation (functionName, parameters) -> SameState
      
    
    let checkTypes (root:AST): Result<PrimitiveType> =
      Success (SimplePrimitive Primitive.Int)


    let rec isInScope (scopeToCheckIfIn:Scope) (otherScope:Scope) : bool =
        otherScope = scopeToCheckIfIn || match otherScope.outer with
                                          | None -> false
                                          | Some s -> isInScope scopeToCheckIfIn s

    let rec isInSameScope (entries:SymbolTable) : bool =
        match entries with
        | [] -> false
        | x::xs ->
            xs
            |> List.exists (fun elem -> isInScope x.scope elem.scope)  //(fun elem -> elem.scope = x.scope)
            //|> List.map (fun elem -> Failure [sprintf "Symbol %s already declared in scope" elem.symbol.identity])
            |> fun x -> x ||  isInSameScope xs

        //Success ()

    let checkHiding (symbolTable:SymbolTable) : Result<SymbolTable> =
        // find alle dupliketter
        // for hver dupliket a, se om dupliketter b, har b.scope.outer = a.scope eller b.scope = a.scope
        // første entry af a: er nogen i samme scope af dem under mig i listen?
        let res = symbolTable
                    |> Seq.groupBy (fun entry -> entry.symbol.identity)
                    |> Seq.filter (fun (key, entries) -> entries |> Seq.length > 1)
                    |> Seq.map (fun (key, entries) -> (key, List.ofSeq entries))
                    |> Seq.filter (fun (key, entries) -> isInSameScope entries)
                    |> Seq.map (fun (key, _) ->
                              Failure [sprintf "Symbol \"%A\" declared multiple times in same scope." key])
                    |> List.ofSeq

        match res with
        | [] -> Success symbolTable
        | xs -> addResults xs

    (* ------------------------- Pure AST forsøg ------------------------------ *)
    type Symbols = Map<string, PrimitiveType> list

    let findInSymbols (symbols:Symbols) (needle:string) : PrimitiveType option =
        symbols
        |> List.tryPick (fun symMap -> symMap.TryFind needle)

    let addSymbol (sym:string) (value:PrimitiveType) : State<unit, Symbols> =
        state {
            let! s = getState
            let newS = s.Head.Add (sym, value)
            do! putState (newS :: s.Tail)
        }

    // applies a state workflow to all elements in list
    let rec forAll1 (p:('a ->State<'a,'b>)) (list:'a list) : State<'a list, 'b> =
        state {
            match list with
            | [] -> return []
            | x::xs ->
                let! x1 = p x
                let! xs1 = forAll1 p xs
                return x1 :: xs1
              }

    let openScope1 : State<unit, Symbols> =
        state {
                let! state = getState
                do! putState (Map.empty :: state)
              }

    let closeScope1 : State<unit, Symbols> =
        state {
                let! state = getState
                do! putState state.Tail
              }

    let tryFindRealType (symbols:Symbols) (primType:PrimitiveType) : PrimitiveType option =
        match primType with
        | UserType str ->
            match findInSymbols symbols str with
            | Some x as res -> res
            | None -> failwith (sprintf "Could not find type %s" str)
        | other -> Some other

    let rec expandUserTypes (root:AST) : State<AST, Symbols> =
        match root with
        | Program bodies  ->
            state {
                do! openScope1
                let! newBodies = forAll1 expandUserTypes bodies
                do! closeScope1
                return ( Program newBodies)
            }
        | Body bodies ->
            state {
                do! openScope1
                let! newBodies = forAll1 expandUserTypes bodies
                do! closeScope1
                return ( Body newBodies)
            }
            //Body ( bodies |> List.map (expandUserTypes symbols) )
        | Block bodies ->
            state {
                do! openScope1
                let! newBodies = forAll1 expandUserTypes bodies
                do! closeScope1
                return ( Block newBodies)
            }
            //Block ( bodies |> List.map (expandUserTypes symbols) )
        | Actor (name, body) ->
            state {
                do! openScope1
                let! newBody = expandUserTypes body
                do! closeScope1
                return (Actor (name, newBody))
            }
            (*let newBody = expandUserTypes symbols body
            Actor (name, newBody)*)
        | Assignment (mutability, varId, value) ->
            state {
                let! symbols = getState
                match findInSymbols symbols varId  with
                | Some _ ->
                    let! newValue = expandUserTypes value
                    return ( Assignment (
                                        mutability,
                                        varId,
                                        newValue))
                | None -> return failwith "assignment expects variable to already be declared"
            }
        | Constant (type', value) as ast ->
            state {
                return ast
            }
        | ForIn (counterName, list, body) ->
            state {
                do! openScope1
                let! symbols = getState
                match findInSymbols symbols counterName  with
                | Some _ ->
                    do! closeScope1
                    return failwith "counter in forIn already declared"
                | None ->
                    do! addSymbol counterName (SimplePrimitive Primitive.Int) // TODO: get the type of counter from list
                    let! newList = expandUserTypes list
                    let! newBody = expandUserTypes body
                    do! closeScope1
                    return ForIn (counterName, newList, newBody)
            }

        | Function (funcName, arguments, types, body) ->
            state {
                let! symbols = getState
                // TODO : Lige nu tjekkes der ikke for om argumenterne allerede er deklæreret
                match findInSymbols symbols funcName  with
                | Some _ ->
                    return failwith "Function already declared"
                | None ->
                    // TODO : tilføj arguments og types til symbols
                    let newTypes = match types with
                                   | ArrowPrimitive prims ->
                                       prims
                                       |> List.map (
                                          fun prim -> match tryFindRealType symbols prim with
                                                      | Some x -> x
                                                      | None -> failwith (sprintf "Could not find real type of %A" prim))
                                       |> ArrowPrimitive
                                   | prim -> match tryFindRealType symbols prim with
                                                             | Some x -> x
                                                             | None -> failwith (sprintf "Could not find real type of %A" prim)

                    do! addSymbol funcName newTypes
                    do! openScope1
                    let! newBody = expandUserTypes body
                    do! closeScope1
                    return Function (funcName, arguments, newTypes, newBody)
            }

        | Identifier id as ast ->
            state {
                return ast
            }
        | If (conditional, body) ->
            state {
                let! newCond = expandUserTypes conditional
                let! newBody = expandUserTypes body
                return If (newCond, newBody)
            }
        | Invocation (functionName, parameters) ->
            state {
                let! symbols = getState
                match findInSymbols symbols functionName  with
                | Some _ -> return Invocation (functionName, parameters) // TODO : parameters er en string list. Dvs. jeg kan ikke sige f(5)
                | None -> return failwith "Cannot invoke function that is not declared"
            }
        | ListRange content ->
            state {
                let! newContent = forAll1 expandUserTypes content
                return ListRange newContent
            }
        | Operation (lhs, op, rhs) ->
            state {
                return Operation (lhs, op, rhs)
            }
        | Reassignment (varId, rhs) ->
            state {
                let! symbols = getState
                match varId with
                | SimpleIdentifier x ->
                    match findInSymbols symbols x  with
                    | Some _ ->
                        let! newRhs = expandUserTypes rhs
                        return Reassignment (varId, newRhs)
                    | None -> return failwith "Reassigning variable that is not declared"
                | IdentifierAccessor xs ->
                    // TODO: hvad dælen skal vi her? kun kigge på head af accessor?
                    return Reassignment (varId, rhs)
            }
        | Receive (msgName, msgType, body) ->
            state {
                do! openScope1
                let! symbols = getState
                let newMsgType = match msgType with
                                 | UserType "args" ->
                                     (SimplePrimitive (Primitive.Struct ("args", [("argv", ListPrimitive (SimplePrimitive Primitive.Char));("argc", SimplePrimitive Primitive.Int)])))
                                 | other ->
                                     match tryFindRealType symbols msgType with
                                     | Some x -> x
                                     | None -> failwith (sprintf "Could not find real type of %A" msgType)
                let! newBody = expandUserTypes body
                do! closeScope1
                return Receive (msgName, newMsgType, newBody)
            }
        | Send (actorName, msgName) ->
            state {
                return Send (actorName, msgName)
            }
        | Spawn (lvalue, actorName, initMsg) ->
            state {
                let! symbols = getState
                let {identity = name; isMutable = mutability; primitiveType = primType} = lvalue
                let newPrimType = match findInSymbols symbols actorName with
                                  | Some x ->
                                      match tryFindRealType symbols primType with
                                      | Some x -> x
                                      | None -> failwith (sprintf "Could not find real type of %A" primType)
                                  | None -> failwith "Trying to spawn actor that is not yet declared"
                let newLValue = {lvalue with primitiveType = newPrimType}
                return Spawn (newLValue, actorName, initMsg)
            }
        | Struct (name, fields) ->
            state {
                return Struct (name, fields)
            }
        | Initialisation (lvalue, rhs) ->
            state {
                let! symbols = getState
                let {identity = name; isMutable = mutability; primitiveType = primType} = lvalue
                match name with
                | SimpleIdentifier x ->
                    match findInSymbols symbols x  with
                    | Some _ -> return failwith "Initialisation already declared"
                    | None ->
                        let newPrimType = match tryFindRealType symbols primType with
                                          | Some x -> x
                                          | None -> failwith (sprintf "Could not find real type of %A" primType)
                        do! addSymbol x newPrimType
                        let! newRhs = expandUserTypes rhs
                        let newLValue = {lvalue with primitiveType = newPrimType}
                        return Initialisation (newLValue, newRhs)
            }
        | StructLiteral (fields) ->
            state {
                    return StructLiteral (fields)
            }

    let rec findTopLevelSyms (syms:Symbols) (root:AST) : Symbols =
        match root with
        | Program bodies | Body bodies | Block bodies ->
            bodies
            |> List.fold (fun st t -> findTopLevelSyms st t ) syms
        | Actor (name, body) ->
            syms.Head.Add(name, SimplePrimitive (Primitive.Actor name)) :: syms.Tail
        | Struct (name, fields) ->
            syms.Head.Add(name, SimplePrimitive (Primitive.Struct (name, fields))) :: syms.Tail
        | other ->
            syms

    (* ------------------------- Pure AST forsøg end ------------------------------ *)

    let analyse (root:AST) : Result<int> =
      //let symbolTable = (evalState (buildSymbolTable root) {symbolList = []; errors = []; scope = {outer = None; level = []}; scopeCounter = 0}).symbolList
      //checkHiding symbolTable
      //symbolTable
      //|> List.map (fun entry -> (entry.symbol.identity, entry.symbol.primitiveType))
      //|> printfn "%A"
      //typecheck root
      //|> printfn "%A"
      //printfn "%A" root
      //findTopLevelSyms Map.empty root
      //printfn "%A" topLevelSymbols

      // let topLevelSymbols = findTopLevelSyms [Map.empty] root
      // evalState  (expandUserTypes root) topLevelSymbols
      // |> List.map Map.toList
      // |> List.map (printfn "\n%A")
      //|> printfn "%A"
      let topLevelSymbols = findTopLevelSyms [Map.empty] root
      execState  (expandUserTypes root) topLevelSymbols
      |> checkTypes
      //|> printfn "%A"

      Success 0
      (*
      root
      //|> (fun ast -> evalState (analyseSemantic ast) {symbolList=[Map.empty];ast=ast})
      |> createSymbolTable {outer = None; id = 0}
      |> fun symbolTableOption -> match symbolTableOption with
                                  | None -> Failure ["Symbol table generation"]
                                  | Some symbolTable -> Success (symbolTable)
      >>= checkHiding
      >>= (fun _ -> Success 0)
      *)
