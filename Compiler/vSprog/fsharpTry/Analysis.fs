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

    type Scope = {
        outer:Scope option
        level:int list
    }

    type SymbolTableEntry = {
        error:bool
        symbol:LValue
        statementType:StatementType
        scope:Scope
    }

    type SymbolTable = SymbolTableEntry list

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

            (*
            >>= fun primType -> if lvalue.primitiveType = primType then
                                    Success primType
                                else Failure [sprintf "Assignment does not typecheck. Expected type %A, found type %A" lvalue.primitiveType primType]
                                *)
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
            printfn "StructLiteral bliver ikke typechecked ordentligt pt!!!"
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
            | UserType lvalType, Identifier (SimpleIdentifier rvalType) -> 
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
                    do! openScope
                    do! buildSymbolTable body
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
                let lValue = {identity = varId; isMutable = true; primitiveType = match typecheck rhs with
                                                                                  | Success prim -> prim
                                                                                  | Failure errs -> failwith "nej"}
                let entry = {
                             error = false;
                             symbol = lValue;
                             statementType = Ass;
                             scope = scope}
                do! addEntry entry
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
        //| Struct (name, fields) ->


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

    

    

    let analyse (root:AST) : Result<int> = 
      let symbolTable = (evalState (buildSymbolTable root) {symbolList = []; errors = []; scope = {outer = None; level = []}; scopeCounter = 0}).symbolList
      //checkHiding symbolTable
      //|> printfn "%A"
      typecheck root
      |> printfn "%A"
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