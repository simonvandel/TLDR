namespace vSprog

open Hime.CentralDogma;
open Hime.Redist;
open vSprog.CommonTypes
open vSprog.AST
open AnalysisUtils

module Analysis =
    let rec buildSymbolTable (root:AST) :  State<unit, Environment> =
        match root with
        | Block stms | Program stms ->
          state 
            {
              do! forAll buildSymbolTable stms
            }
        | Body stms -> 
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
        | If (condition, body) -> 
          state 
            {
              do! openScope
              do! buildSymbolTable body
              do! closeScope
            }
        | Send (actorName, msgName) -> SameState getState
        | Spawn (lvalue, actorName, initMsg) -> SameState getState
        | Receive (msgName, msgType, body) -> 
          state 
            {
              do! openScope
              do! buildSymbolTable body
              do! closeScope
            }
        | ForIn (counterName, list, body) -> 
          state 
            {
              do! openScope
              do! buildSymbolTable body
              do! closeScope
            }
        | ListRange content -> SameState getState
        | BinOperation (lhs, op, rhs) -> 
          state
            {
              do! buildSymbolTable lhs
              do! buildSymbolTable rhs
            }
        | Identifier id -> SameState getState
        | Function (funcName, arguments, types, body) -> 
          state 
            {
              do! openScope
              do! buildSymbolTable body
              do! closeScope
            }
        | StructLiteral fieldNamesAndVals -> 
          state
            {
              do! forAll buildSymbolTable (fieldNamesAndVals |> List.map (fun e -> snd e))
            }
        | Invocation (functionName, parameters) -> SameState getState
        | UnaryOperation (op, rhs) -> 
          state
            {
              do! buildSymbolTable rhs
            }
        | While (cond, body) ->
          state 
            {
              do! openScope
              do! buildSymbolTable body
              do! closeScope
            }
        | Kill (arg) -> SameState getState
        | Me -> SameState getState
        | Return (arg) -> SameState getState
        | IfElse(cond, tBody, fBody) ->
          state
            {
              do! openScope
              do! buildSymbolTable tBody
              do! closeScope

              do! openScope
              do! buildSymbolTable fBody
              do! closeScope
            }
        | Constant (ptype, value)-> SameState getState

    let checkHiding (symbolTable:SymbolTable) : Result<SymbolTable> =
        // find alle dupliketter
        // for hver dupliket a, se om dupliketter b, har b.scope.outer = a.scope eller b.scope = a.scope
        // første entry af a: er nogen i samme scope af dem under mig i listen?
        let mutable res:Result<SymbolTable> list = []
        let NoReass = symbolTable |> List.filter (fun e -> e.statementType <> Reass)
        for i in NoReass do
          let seq = List.filter (fun e -> isVisible i e && i.symbol.identity = e.symbol.identity && i <> e && e.statementType <> Reass) symbolTable
          if seq.Length > 0 then 
            res <- Failure [sprintf "Element %A hides %A" i res] :: res

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

    let checkReass symTable = 
      //printfn "%A" symTable
      let res = symTable |> 
                  List.filter (fun entry -> entry.statementType = Reass) |>
                    List.filter (fun entry -> entry.symbol.isMutable = false) |>
                      List.map (fun e -> Failure [sprintf "Invalid reassignment, %A is not mutable" e.symbol.identity.ToString])
      match res with
      | [] -> Success symTable
      | xs -> sumResults xs

    let analyse (ast:AST) : Result<AST> = 
        printfn "%A" ast
        //let environment = evalState (buildSymbolTable ast) {symbolList = []; errors = []; scope = {outer = None; level = []}; scopeCounter = 0}
        //environment |> printf "%A"
        //environment |> (fun env -> checkReass env.symbolList)
        Success ast
