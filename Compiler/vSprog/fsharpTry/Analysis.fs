namespace vSprog

open Hime.CentralDogma;
open Hime.Redist;
open vSprog.CommonTypes
open vSprog.AST

module Analysis =


    (*
    type Val =
        | Int of int
        | Real of float
        | Char of char
        | Bool of bool
        | List of Val list

    type TypeSymbol = string * Val // (id, value)
        
    *)

    type StatementType = 
        | Decl
        | Init
        | Ass
        | Reass

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
        symbolList: SymbolTable //Map<string, TypeSymbol> list
        errors: string list
        scope: Scope
        scopeCounter: int
        //ast:AST
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
        | Assignment (lValue, primitiveType) ->
            state {
                let! scope = getScope
                let entry = {
                             error = false;
                             symbol = lValue;
                             statementType = Ass;
                             scope = scope}
                do! addEntry entry
            }


    let mergeSymbolTables sym1 sym2 : SymbolTable =
        List.append sym1 sym2

    let getValidSymbolTables symbolTableOptions = 
        symbolTableOptions
            |> List.filter Option.isSome
            |> List.map Option.get

    let rec createSymbolTable (currentScope:Scope) (ast:AST) : SymbolTable option =
        match ast with
            | Program statements -> 
                Some (statements
                    |> List.map (createSymbolTable currentScope)
                    |> getValidSymbolTables
                    |> List.reduce mergeSymbolTables)
            | Block statements ->
                Some (statements
                    |> List.map (createSymbolTable {outer = Some currentScope; level = currentScope.level})
                    |> getValidSymbolTables
                    |> List.reduce mergeSymbolTables)
            | Assignment (lValue, primitiveType) -> 
                    let entry = {error = false; symbol = lValue; statementType = Ass; scope = currentScope}
                    match primitiveType with
                        | Node ast -> match createSymbolTable {outer = Some currentScope; level = currentScope.level} ast with
                                            | None -> Some [entry]
                                            | Some a -> Some (entry :: a)
                        | _ -> Some [entry]
            | _ -> None

    (*

    let retrieveSymbol (name:string) : State<TypeSymbol option, Environment> =
        state {
                let! state = getState
                return None

                //let currentSymbols = state.symbolTable.symbols

                (*

                // get all symbols visible, and try to find the given symbol
                let res = Seq.unfold (fun (scope:Scope option) -> 
                                                        match scope with
                                                        | None -> None
                                                        | Some a -> Some (a.symbols, a.parent)) state.symbolTable.parent
                                |> List.ofSeq
                                |> fun list -> currentSymbols :: list
                                |> List.tryPick (fun map -> map.TryFind name)
                return res
                *)
              }

    let declaredLocally (name:string) : State<bool, Environment> =
        state {
                let! state = getState
                return false
                //return state.symbolTable.symbols.ContainsKey name
              }

    *)





    (*let toEx (ast:ASTNode) : Ex =
        match ast.Symbol.Value with
        | value when System.Int32.TryParse(value,ref 0) -> Int (System.Int32.Parse(value))
        | value when System.Double.TryParse(value,ref 0.0) -> Real (float value)
        | value -> // must be variable
            let id = value
            let typeId = (ast.Children.Item 0).Symbol.Value
            Variable (id, typeId)*)
    let (|Integer|_|) (str: string) =
        let mutable intvalue = 0
        if System.Int32.TryParse(str, &intvalue) then Some(intvalue)
        else None
    
    let (|Character|_|) (str: string) =
        if str.StartsWith "'" && str.EndsWith "'" && str.Length = 3 then Some(str.[1])
        else None
    
    let (|Real|_|) (str: string) =
        let mutable realvalue = 0.0
        if System.Double.TryParse(str, &realvalue) then Some(realvalue)
        else None

    let (|Bool|_|) (str: string) =
        let mutable realvalue = 0.0
        match str with
            | "true" -> Some(true)
            | "false" -> Some(false)
            | _ -> None

    let (|UserType|_|) (str: string) =
        let mutable realvalue = 0.0
        match str with
            | "true" -> Some(true)
            | "false" -> Some(false)
            | _ -> None


    let rec analyseSemantic (ast:AST) : State<unit,Environment> =
        match ast with
        | Program stms -> 
            state {
                      do! forAll analyseSemantic stms
                  }
        | Block stms -> 
            state {
                      do! forAll analyseSemantic stms
                  }
        | Assignment (lhs, rhs) -> 
            state {
                      (*
                      let (symbolName, symbolVal:Val) = 
                           match (lhs, rhs) with
                           | Variable (name, type') , Int i -> 
                              (name, Val.Int i)
                           | Variable (name, type') , Real r -> 
                              (name, Val.Real r)
                           | _ as t , value ->
                              printfn "%s %A" "NOT MATCHED" t 
                              ("not matched", Val.Bool true)
                      do! enterSymbol symbolName symbolVal
                      *)
                      return ()
                  }

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
                              Failure [sprintf "Symbol \"%s\" declared multiple times in same scope." key])
                    |> List.ofSeq

        match res with
        | [] -> Success symbolTable
        | xs -> addResults xs


    

    let typecheck (lhs:PrimitiveType) (rhs:PrimitiveType) : Result<unit> =
        match lhs, rhs with
        //| lhs', Node ast -> 
        | lhs', rhs' -> 
            if lhs' = rhs' 
            then Success ()
            else Failure [sprintf "%A does not have same type as %A" lhs' rhs']


    let rec typechecker (root:AST) : Result<AST> =
        match root with
        | Program stms -> 
            stms 
            |> List.map typechecker
            |> addResults
        | Block stms ->
            stms 
            |> List.map typechecker
            |> addResults
        | Assignment (lvalue, primitiveType) -> 
            typecheck lvalue.primitiveType primitiveType
            >>= fun _ -> Success root
        | other -> Failure [sprintf "&s not typechecked"]

    let analyse (root:AST) : Result<int> = 
      let symbolTable = (evalState (buildSymbolTable root) {symbolList = []; errors = []; scope = {outer = None; level = []}; scopeCounter = 0}).symbolList
      //checkHiding symbolTable
      //|> printfn "%A"
      typechecker root
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