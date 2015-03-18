namespace vSprog

module CommonTypes =


    // Result --------------------------------------------
    type Result<'a> = Choice<'a, string list>
    let Success x : Result<'a> = Choice1Of2 x
    let Failure x : Result<'a> = Choice2Of2 x
    let  (|Success|Failure|) = 
        function Choice1Of2 x -> Success x | Choice2Of2 x -> Failure x

    // State --------------------------------------------
    type State<'a, 's> = State of ('s -> 'a * 's) 


    let getState = State (fun s -> (s,s))
    let putState newState = State (fun _ -> (), newState)


    let runState (State s) initState = s initState // return both value and state
    let execState s initState = runState s initState |> fst // return value
    let evalState s initState = runState s initState |> snd // return state

    type StateBuilder() =
        member this.Bind(m, f) = 
            State (fun s ->
                let (firstVal, newState) = runState m s
                in
                runState (f firstVal) newState
                )
        member this.Return(x) = State (fun s -> (x,s))

    let state = new StateBuilder()