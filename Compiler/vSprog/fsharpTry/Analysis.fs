namespace vSprog

open Hime.CentralDogma;
open Hime.Redist;
open vSprog.CommonTypes
open vSprog.AST
open AnalysisUtils

module Analysis =
    let rec buildSymbolTable (root:AST) :  State<unit, Environment> =
        match root with
        | Program stms | Block stms | Body stms-> 
          state 
            {
              do! openScope
              do! forAll buildSymbolTable stms
              do! closeScope
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
                                      | None -> HasNoType
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
                        | None -> HasNoType
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
        | Constant (ptype, value)-> SameState getState
        | If (condition, body) -> SameState getState
        | Send (actorName, msgName) -> SameState getState
        | Spawn (lvalue, actorName, initMsg) -> SameState getState
        | Receive (msgName, msgType, body) -> SameState getState
        | ForIn (counterName, list, body) -> SameState getState
        | ListRange content -> SameState getState
        | BinOperation (lhs, op, rhs) -> SameState getState
        | Identifier id -> SameState getState
        | Function (funcName, arguments, types, body) -> SameState getState
        | StructLiteral fieldNamesAndVals -> SameState getState
        | Invocation (functionName, parameters) -> SameState getState
      
    let checkHiding (symbolTable:SymbolTable) : Result<SymbolTable> =
        // find alle dupliketter
        // for hver dupliket a, se om dupliketter b, har b.scope.outer = a.scope eller b.scope = a.scope
        // første entry af a: er nogen i samme scope af dem under mig i listen?
        let res = symbolTable
                    |> Seq.groupBy (fun entry -> entry.symbol.identity)
                    |> Seq.map (fun (key, entries) -> (key, List.ofSeq entries))
                    |> Seq.filter (fun (key, entries) -> isInSameScope entries)
                    |> Seq.map(fun (key, entries) -> (key, entries |> List.filter(fun e -> e.statementType <> Reass)))
                    |> Seq.filter (fun (key, entries) -> entries |> Seq.length > 1)
                    |> Seq.map (fun (key, _) ->
                              Failure [sprintf "Symbol \"%A\" declared multiple times in same scope." key])
                    |> List.ofSeq

        match res with
        | [] -> Success symbolTable
        | xs -> sumResults xs

    let checkUsedBeforeDecl (symTable:SymbolTable) : Result<SymbolTable> =
      let res = symTable |>
                    List.filter (fun e -> e.symbol.primitiveType = PrimitiveType.HasNoType ) |>
                      List.map (fun e -> Failure [sprintf "Symbol \"%A\" is used before its declaration." e.symbol.identity.ToString])
      match res with
      | [] -> Success symTable
      | xs -> sumResults xs

    let checkTypes (root:AST) (symTable:SymbolTable): Result<PrimitiveType> =
      Success (SimplePrimitive Primitive.Int)

    let checkReass (symTable:SymbolTable) : Result<unit> = 
        //printfn "%A" symTable
        
        symTable 
        |> List.filter (fun entry -> entry.statementType = Reass)
        |> printfn "%A"
        Success ()

    let analyse (ast:AST) : Result<AST> = 
        printfn "%A" ast
        //let environment = evalState (buildSymbolTable ast) {symbolList = []; errors = []; scope = {outer = None; level = []}; scopeCounter = 0}
        //environment |> printf "%A"
        //environment |> (fun env -> checkReass env.symbolList)
        Success ast
