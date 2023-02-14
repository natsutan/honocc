module Genx86

open System.IO
open Honodef

let mutable stack_count = 0

let push (fp : StreamWriter) =
    stack_count <- stack_count + 1
    fp.WriteLine "  push %rax"
    

let pop(fp : StreamWriter, reg) =
    stack_count <- stack_count - 1
    fp.WriteLine $"  pop %s{reg}"

let number(fp : StreamWriter, num:NdNum) =
    fp.WriteLine $"  mov $%d{num.Value}, %%rax "
    push fp

let expr(fp, ast) =
    match ast with
    | Num(node_num) -> number(fp, node_num)
    | _ -> failwith $"unsupported node  %A{ast}"
    
let gen_funccall(fp, funccall: NdFuncCall) =
    // パラメータは１つのみ
    expr(fp, funccall.Params.Head)
    
    //パラメータ一つの時はrdiに値を入れる。    
    pop(fp, "%rdi")
    //関数呼び出し
    fp.WriteLine $"  call %s{funccall.Name}"
    
    


let gen_body(fp, asts: Ast list) =
    for ast in asts do
        match ast with
        | Ast.FuncCall(funcall) -> gen_funccall(fp, funcall)
        | _ -> failwith $"unsupported node  %A{ast}"
    

let generate(asm_file_path : string,  func : NdFunction)=
     let fp = new StreamWriter(asm_file_path)
     
     fp.WriteLine $".globl {func.Name}"
     fp.WriteLine ".text"
     fp.WriteLine $"{func.Name}:"

     fp.WriteLine "# Prologue"    
     fp.WriteLine "  push %rbp"        //ベースポインタを保存
     fp.WriteLine "  mov %rsp, %rbp"     //ベースポインタに関数に入った時のスタックポインタを保存
     fp.WriteLine $"  sub $%d{0}, %%rsp"     //変数の領域確保
     fp.WriteLine ""  
     
     gen_body(fp, func.Body)  |> ignore
     assert (stack_count = 0)
     
     fp.WriteLine "# Epilogue"
     
     fp.WriteLine $".L.return.%s{func.Name}:"        
     fp.WriteLine "  mov %rbp, %rsp"      // スタックポインタの復元
     fp.WriteLine "  pop %rbp"     //ベースポインタの復元
     fp.WriteLine "  ret"  
     fp.WriteLine ""  
     
     fp.Close()
     
     