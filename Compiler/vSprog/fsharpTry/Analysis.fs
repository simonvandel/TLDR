namespace vSprog

open Hime.CentralDogma;
open Hime.Redist;
open vSprog.CommonTypes

module Analysis =

    type Val =
        | Int of int
        | Real of float
        | Char of char
        | Bool of bool
        | List of Val list

    type TypeSymbol = string * Val // (id, value)
        
    type Primitive =
        | Int
        | Char
        | Real
        | Bool

    type PrimitiveType =
        | SimplePrimitive of Primitive
        | ListPrimitive of PrimitiveType
        | Node of AST
        | UserType of string

    and AST = 
        | Program of AST list
        | Block of AST list
        | Assignment of LValue * PrimitiveType
        | Error // Only for making it compile temporarily

    and LValue = {
        identity:string
        isMutable:bool
        primitiveType:PrimitiveType
    }

    type StatementType = 
        | Decl
        | Init
        | Ass
        | Reass 

    type Scope = {
        outer:Scope
    }

    type SymbolTableEntry = {
        error:bool
        symbol:LValue
        statementType:StatementType
        scope:Scope
    }

    type SymbolTable = SymbolTableEntry list

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
                    |> List.map (createSymbolTable {outer = currentScope})
                    |> getValidSymbolTables
                    |> List.reduce mergeSymbolTables)
            | Assignment (lValue, primitiveType) -> 
                    let entry = {error = false; symbol = lValue; statementType = Ass; scope = currentScope}
                    match primitiveType with
                        | Node ast -> match createSymbolTable {outer = currentScope} ast with
                                            | None -> Some [entry]
                                            | Some a -> Some (entry :: a)
                        | _ -> Some [entry]
            | _ -> None

    type Environment =  {
        symbolTable: Scope
        //visibleScope: Scope
    }

    let openScope : State<unit, Environment> = 
        state {
                let! state = getState
                return ()
                // TODO add to symbolTable and update visibleScope

                //let newScope = {parent = Some state.symbolTable; symbols = Map.empty;}


                //let newState = {state with
                    //              symbolTable = newScope
                  //             }
                //do! putState newState
              }

    let closeScope : State<unit, Environment> = 
        state {
                let! state = getState
                (*

                let newState = {state with
                                 symbolTable = match state.symbolTable.outer with
                                               | None -> failwith "Could not close a scope, because you are already in outermost scope"
                                               | Some a -> a
                               }

                do! putState newState
                *)
                return ()

              }

    let enterSymbol (name:string) (type':Val) : State<unit, Environment> =
        state {
                let! state = getState
                return ()
                //let addSymbol = fun (scope:Scope) -> {scope with
                  //                                     symbols = scope.symbols.Add (name, (name, type'))
                    //                                 }
                //let newState = {state with
                  //               symbolTable = addSymbol state.symbolTable
                    //           }

                //do! putState newState
              }

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

    let rec toPrimitiveType (input:string) : PrimitiveType =
        match input with
            | "int" -> SimplePrimitive Int
            | "char" -> SimplePrimitive Char
            | "real" -> SimplePrimitive Real
            | "bool" -> SimplePrimitive Bool
            | str when str.StartsWith("[") && str.EndsWith("]") -> ListPrimitive (toPrimitiveType (str.Substring (1, input.Length-2)))
            | str -> UserType str

    let toLValue (mutability:ASTNode) (name:ASTNode) (typeName:ASTNode) : LValue = 
        let isMutable = match mutability.Symbol.Value with
                            | "let" -> false
                            | "var" -> true
        {identity = name.Symbol.Value; 
        isMutable = isMutable;
        primitiveType = toPrimitiveType typeName.Symbol.Value}

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

    let rec toAST (root:ASTNode) : AST =
        match root.Symbol.Value with
        | "StatementList" as state -> 
            printfn "%s %s" "Entered" state
            let t = traverseChildren root
            printfn "%s %s" "Left" state
            Program t
        | "Statement" as state -> 
            printfn "%s %s" "Entered" state
            traverseChildren root
            printfn "%s %s" "Left" state
            Error
        | "Term" as state -> 
            printfn "%s %s" "Entered" state
            traverseChildren root
            printfn "%s %s" "Left" state
            Error
        | "Block" as state -> 
            printfn "%s %s" "Entered" state
            let t =traverseChildren root
            printfn "%s %s" "Left" state
            Block t
        | "Initialisation" as state ->
            printfn "%s %s" "Entered" state
            //traverseChildren root
            let lhs = toLValue (root.Children.Item 0) (root.Children.Item 1) ((root.Children.Item 1).Children.Item 0)
            let rhs = Node (toAST (root.Children.Item 2))
            printfn "%s %s" "Left" state
            Assignment (lhs, rhs)
        | sym -> 
            printfn "%s%A" "ERROR: No match case for: " sym
            Error

    and traverseChildren (root:ASTNode) : AST list =
        List.ofSeq (seq { for i in root.Children -> i})
            |> List.map toAST

    let addResults (results:Result<'a> list) : Result<'a> =
        results
        |> List.reduce (fun accum elem -> match accum, elem with
                                                | Failure msg1, Failure msg2 -> Failure (msg1 @ msg2)
                                                | Failure msg, Success _ -> Failure msg
                                                | Success a, Success b -> Success b
                                                | Success a, Failure msg -> Failure msg
                                                )
    
    // applies a state workflow to all elements in list
    let rec many (p:('a ->State<Result<unit>,'b>)) (list:'a list) : State<Result<unit> list, 'b> =
        state {
            match list with
            | [] -> return [Success ()]
            | x::xs ->
                let! z = p x
                let! zs = many p xs
                return z::zs
              }

    let isAlreadyDeclared (name:string) : State<Result<unit>, Environment> =
        state {
                let! state = getState
                (*
                let currentSymbols = state.symbolTable.symbols.Remove name // do not look for the symbol we are checking for

                // get all symbols visible, and try to find the given symbol
                let res = Seq.unfold (fun (scope:Scope option) -> 
                                                        match scope with
                                                        | None -> None
                                                        | Some a -> Some (a.symbols, a.parent)) state.symbolTable.parent
                                |> List.ofSeq
                                |> fun list -> currentSymbols :: list
                                |> List.tryPick (fun map -> map.TryFind name)

                match res with
                | None -> return Success ()
                | Some a -> return Failure [(sprintf "%A already declared" a)]
                *)
                return Failure ["haha"]
              } 
        
    let alreadyDeclaredCheck : State<Result<unit>, Environment> =
        state {
                let! state = getState
                //let allSymbols = state.symbolTable.symbols |> Map.toList |> List.map fst // get all keys
                //let! res = many isAlreadyDeclared allSymbols
                //return res |> addResults
                return Failure ["hah"]
              }

    let rec analyseSemantic (ast:AST) : State<Result<unit>,Environment> =
        match ast with
        | Program stms -> 
            state {
                      let! results = many analyseSemantic stms
                      return results |> addResults
                  }
        | Block stms -> 
            state {
                      do! openScope
                      let! results = many analyseSemantic stms
                      let! declaredCheck = alreadyDeclaredCheck
                      do! closeScope
                      return (declaredCheck :: results ) |> addResults
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
                      return Failure ["sh"]
                  }

    let analyse (root:ASTNode) : Result<int> = 
      toAST root 
      
      //|> (fun ast -> evalState (analyseSemantic ast) {symbolList=[Map.empty];ast=ast})
      |> printfn "%A"
      Success 0
