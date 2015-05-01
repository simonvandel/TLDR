namespace vSprog

module CommonTypes =


    // Result --------------------------------------------
    type Result<'a> = Choice<'a, string list>
    let Success x : Result<'a> = Choice1Of2 x
    let Failure x : Result<'a> = Choice2Of2 x
    let  (|Success|Failure|) = 
        function Choice1Of2 x -> Success x | Choice2Of2 x -> Failure x

    let sumResults (results:Result<'a> list) : Result<'a> =
        results
        |> List.reduce (fun accum elem -> match accum, elem with
                                                | Failure msg1, Failure msg2 -> Failure (msg1 @ msg2)
                                                | Failure msg, Success _ -> Failure msg
                                                | Success a, Success b -> Success b
                                                | Success a, Failure msg -> Failure msg
                                                )

    let (>>=) m f =
        match m with
        | Success r -> r |> f
        | Failure errs -> Failure errs

    // State --------------------------------------------
    type State<'a, 's> = State of ('s -> 'a * 's)

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

    let getState = State (fun s -> (s,s))
    let putState newState = State (fun _ -> (), newState)