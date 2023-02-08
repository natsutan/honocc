// For more information see https://aka.ms/fsharp-console-apps
open System.IO

let ast_dumpfile = "work/ast.md"
let work_dir = "work"

let toAsmFileName (src_path :string) =
    let src_base_name  = Path.GetFileNameWithoutExtension(src_path)
    src_base_name + ".s"


[<EntryPoint>]
let main args =
    if args.Length <> 1 then
        printfn "Usage: honocc input_file"
        exit 1
    // Return 0. This indicates success.
    let filename = args[0]
    
    // Tokenize
    let token_stream = Tokenizer.tokenizeFromFile filename
    token_stream.debPrintTokens()

    // Parse
    let func = Parser.parse token_stream
    Astdump.dump(func, ast_dumpfile) |> ignore
    
    // Generate
    let asm_filename = toAsmFileName filename
    let asm_file_path = Path.Combine(work_dir, asm_filename)
    
    Genx86.generate(asm_file_path, func) |> ignore
    
    
    printfn $"write to %s{asm_file_path}"
    0