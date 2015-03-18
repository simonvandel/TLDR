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

        type Ex =
        | Variable of string * string // * Scope // (name, type)
        | Int of int
        | Real of float

    type AST = 
        | Program of AST list
        | Block of AST list
        | Assignment of Ex * Ex
        | Error // Only for making it compile temporarily

    type Environment =  {
        symbolList: Map<string, TypeSymbol> list
        ast:AST
    }

    let openScope : State<unit, Environment> = 
        state {
                let! state = getState
                let newState = {state with symbolList = Map.empty :: state.symbolList}
                do! putState newState
              }

    let closeScope : State<unit, Environment> = 
        state {
                let! state = getState
                do! putState ( match state.symbolList with
                              | [] -> state
                              | _::xs -> {state with symbolList = xs}
                )
              }

    let enterSymbol (name:string) (type':Val) : State<unit, Environment> =
        state {
                let! state = getState
                let newState = {state with symbolList =  state.symbolList.Head.Add (name, (name, type')) ::  state.symbolList.Tail}
                do! putState newState
              }

    let retrieveSymbol (name:string) : State<TypeSymbol option, Environment> =
        state {
                let! state = getState
                do! putState state

                let res = state.symbolList
                          |> List.tryPick (fun map -> map.TryFind name)

                return res
              }

    let declaredLocally (name:string) : State<bool, Environment> =
        state {
                let! state = getState
                return state.symbolList.Head.ContainsKey name
              }

    type Scope = 
        {
            SymbolList:TypeSymbol list
            Parent:Scope
        }

    let toEx (ast:ASTNode) : Ex =
        match ast.Symbol.Value with
        | value when System.Int32.TryParse(value,ref 0) -> Int (System.Int32.Parse(value))
        | value when System.Double.TryParse(value,ref 0.0) -> Real (float value)
        | value -> // must be variable
            let id = value
            let typeId = (ast.Children.Item 0).Symbol.Value
            Variable (id, typeId)

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
            let lhs = toEx (root.Children.Item 1)
            let rhs = toEx (root.Children.Item 2)
            printfn "%s %s" "Left" state
            Assignment (lhs, rhs)
        | sym -> 
            printfn "%s%A" "ERROR: No match case for: " sym
            Error

    and traverseChildren (root:ASTNode) : AST list =
        List.ofSeq (seq { for i in root.Children -> i})
            |> List.map toAST

    // applies a state workflow to all elements in list
    let rec forAll (p:('a ->State<unit,'b>)) (list:'a list) : State<unit, 'b> =
        state {
            match list with
            | [] -> return ()
            | x::xs ->
                do! p x
                do! forAll p xs
              }

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
                  }

    let analyse (root:ASTNode) : Result<int> = 
      toAST root 
      |> (fun ast -> evalState (analyseSemantic ast) {symbolList=[Map.empty];ast=ast})
      |> printfn "%A"
      Success 0
