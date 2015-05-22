module AnalysisUtils
open vSprog.CommonTypes
open vSprog.AST

    type StatementType =
        | Init // var x:int := 5
        | Reass // x := 5
        | Def // Actor A := {} | Struct B := {}
        | Use // x

    type Scope = {
        outer:Scope option //TODO: fjern outer, da det bare kan ses ud fra level
        level:int list
    }

    [<ReferenceEquality>]
    type SymbolTableEntry = {
        symbol: LValue
        statementType:StatementType
        scope:Scope
        value:AST
    }

    type SymbolTable = SymbolTableEntry list //Map<Scope, Map<string, PrimitiveType>>  //

    type Environment =  {
        symbolList: SymbolTable
        errors: string list
        scope: Scope
        scopeCounter: int
        ast: AST
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
                let newState = {state with scope = {outer = Some state.scope; level = state.scope.level @ [newScopeCtr]}
                                           scopeCounter = newScopeCtr
                                }
                do! putState newState
              }

    let closeScope : State<unit, Environment> =
        state {
                let! state = getState
                let newState = {state with 
                                      scope = {outer = Option.bind (fun sc -> sc.outer) state.scope.outer; 
                                      level = state.scope.level |> List.rev |> List.tail |> List.rev}
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

    let SameState currentState = 
      state 
        {
          let! curState = currentState
          do! putState curState
        }

    let rec isInScope (scopeToCheckIfIn:Scope) (perspective:Scope) : bool =
        perspective = scopeToCheckIfIn || match perspective.outer with
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

    //Takes two symbolTableEntries and checks that the second is visible to the first
    let isVisible (persp:SymbolTableEntry) (obj:SymbolTableEntry) : bool =
      let mutable matches:bool = true
      if persp.scope.level.Length < obj.scope.level.Length then
        matches <- false

      let zippedList = Seq.zip persp.scope.level obj.scope.level
      for i in zippedList do
        if fst i <> snd i then
          matches <- false
      matches
