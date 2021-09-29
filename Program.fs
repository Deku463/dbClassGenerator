open Args

[<EntryPoint>]
let main argv =
    makeFile argv |> ignore
    0 // return an integer exit code
