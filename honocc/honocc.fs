open System.IO

let ast_dumpfile = "work/ast.md"
let mutable work_dir_x64 = "work_x64"
let mutable work_dir_riscv = "work_riscv"

let toAsmFileName (src_path :string) =
    let src_base_name  = Path.GetFileNameWithoutExtension(src_path)
    src_base_name + ".s"


// See https://jyuch.hatenablog.com/entry/2017/08/30/233000
type Param = { OutputDirX64:string;OutputDirRiscv:string; File:string }

[<EntryPoint>]
let main args =
    
    let rec parseImpl input param =
                match input with
                | "--output-dir-x64" :: dir :: tail ->
                    parseImpl tail { param with OutputDirX64 = dir }
                | "--output-dir-riscv" :: dir :: tail ->
                    parseImpl tail { param with OutputDirRiscv = dir }
                | file :: tail ->
                    parseImpl tail { param with File = file }
                | [] -> param
        
    let parse input = parseImpl (Array.toList input) {OutputDirX64 = work_dir_x64; OutputDirRiscv = work_dir_riscv; File = "" } 
    
    let options = parse args
      
    let filename = options.File
    work_dir_x64 <- options.OutputDirX64
    work_dir_riscv <- options.OutputDirRiscv
   
       
    // Tokenize
    let token_stream = Tokenizer.tokenizeFromFile filename
    //token_stream.debPrintTokens()

    // Parse
    let func = Parser.parse token_stream
    // Astdump.dump(func, ast_dumpfile) |> ignore
    
    // Generate
    let asm_filename = toAsmFileName filename
    let asm_file_path_x64 = Path.Combine(work_dir_x64, asm_filename)
    Genx86.generate(asm_file_path_x64, func) |> ignore
    printfn $"write to %s{asm_file_path_x64}"
    
    let asm_file_path_riscv = Path.Combine(work_dir_riscv, asm_filename)
    Riscv.generate(asm_file_path_riscv, func) |> ignore    
    printfn $"write to %s{asm_file_path_riscv}"
    
    
    0
    