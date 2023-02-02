// For more information see https://aka.ms/fsharp-console-apps

let ast_dumpfile = "work/ast.md"


[<EntryPoint>]
let main args =
    if args.Length <> 1 then
        printfn "Usage: honocc input_file"
        exit 1
    // Return 0. This indicates success.
    let filename = args[0]
    
    let token_stream = Tokenizer.tokenizeFromFile filename
    token_stream.debPrintTokens()
    let func = Parser.parse token_stream
    Astdump.dump(func, ast_dumpfile) |> ignore
    0