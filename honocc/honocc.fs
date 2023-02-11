// For more information see https://aka.ms/fsharp-console-apps
open System.IO

let ast_dumpfile = "work/ast.md"
let work_dir_x64 = "work_x64"
let work_dir_riscv = "work_riscv"

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
    let asm_file_path_x64 = Path.Combine(work_dir_x64, asm_filename)
    Genx86.generate(asm_file_path_x64, func) |> ignore
    printfn $"write to %s{asm_file_path_x64}"
    
    let asm_file_path_riscv = Path.Combine(work_dir_riscv, asm_filename)
    Riscv.generate(asm_file_path_riscv, func) |> ignore    
    printfn $"write to %s{asm_file_path_riscv}"
    
    
    0
    