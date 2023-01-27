// For more information see https://aka.ms/fsharp-console-apps

[<EntryPoint>]
let main args =
    if args.Length <> 1 then
        printfn "Usage: honocc input_file"
        exit 1
    // Return 0. This indicates success.
    let filename = args[0]
    
    let l = Tokenizer.tokenizeFromFile(filename)
    Tokenizer.debPrintTokens l |> ignore
    
    0