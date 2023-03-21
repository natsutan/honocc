module Genx86

open System.IO
open System.Numerics
open Honodef

let mutable stack_count = 0
let mutable label_count = 0

let get_label_cnt =
    let tmp = label_count
    label_count <- label_count + 1
    tmp

let push (fp : StreamWriter) =
    stack_count <- stack_count + 1
    fp.WriteLine "  push %rax"
    

let pop(fp : StreamWriter, reg) =
    stack_count <- stack_count - 1
    fp.WriteLine $"  pop %s{reg}"

let number(fp : StreamWriter, num:NdNum) =
    fp.WriteLine $"  mov $%d{num.Value}, %%rax "
    

let rec gen_expr(fp, ast) =
    match ast with
    | Num(node_num) -> number(fp, node_num)
    | BinOp(binop) ->
        // Binopの左オペランドをrdi,右オペランドをraxに入れる。
        gen_expr(fp, binop.r)
        push(fp)
        gen_expr(fp, binop.l)
        pop(fp, "%rdi")
        match binop.op with
            | BinOpKind.Add -> fp.WriteLine "  add %rdi, %rax"
            | BinOpKind.Sub -> fp.WriteLine "  sub %rdi, %rax"
            | BinOpKind.Mult -> fp.WriteLine "  imul %rdi, %rax"
            | BinOpKind.Div ->
                fp.WriteLine "  cqo"
                fp.WriteLine "  idiv %rdi"
            | BinOpKind.Modulo -> 
                fp.WriteLine "  cdq"
                fp.WriteLine "  idiv %rdi"
                fp.WriteLine "  mov %rdx, %rax"
            | BinOpKind.LShift ->
                fp.WriteLine "  mov %rdi, %rcx"
                fp.WriteLine "  shl %cl, %rax"
            | BinOpKind.RShift ->
                fp.WriteLine "  mov %rdi, %rcx"
                fp.WriteLine "  shr %cl, %rax"
            | BinOpKind.BitAnd -> fp.WriteLine "  and %rdi, %rax"
            | BinOpKind.BitOr -> fp.WriteLine "  or %rdi, %rax"
            | BinOpKind.BitXor -> fp.WriteLine "  xor %rdi, %rax"
            | BinOpKind.Equal ->
                fp.WriteLine "  cmp %rdi, %rax"
                fp.WriteLine "  sete %al"
                fp.WriteLine "  movzb %al, %rax"
            | BinOpKind.NotEqual ->
                fp.WriteLine "  cmp %rdi, %rax"
                fp.WriteLine "  setne %al"
                fp.WriteLine "  movzb %al, %rax"
            | BinOpKind.LesserEqual ->
                fp.WriteLine "  cmp %rdi, %rax"
                fp.WriteLine "  setle %al"
                fp.WriteLine "  movzb %al, %rax"
            | BinOpKind.LesserThan ->
                fp.WriteLine "  cmp %rdi, %rax"
                fp.WriteLine "  setl %al"
                fp.WriteLine "  movzb %al, %rax"
            | BinOpKind.GreaterEqual ->
                fp.WriteLine "  cmp %rdi, %rax"
                fp.WriteLine "  setge %al"
                fp.WriteLine "  movzb %al, %rax"
            | BinOpKind.GreaterThan ->
                fp.WriteLine "  cmp %rdi, %rax"
                fp.WriteLine "  setg %al"
                fp.WriteLine "  movzb %al, %rax"
            | BinOpKind.LogicalAnd ->
                 fp.WriteLine "  cmp $0, %rax"
                 fp.WriteLine "  setne %al"
                 fp.WriteLine "  movzb %al, %rax"
                 fp.WriteLine "  cmp $0, %rdi"
                 fp.WriteLine "  setne %dil"
                 fp.WriteLine "  movzb %dil, %rdi"
                 
                 fp.WriteLine "  and %rdi, %rax"
                 fp.WriteLine "  cmp $0, %rax"
                 fp.WriteLine "  setne %al"
                 fp.WriteLine "  movzb %al, %rax"
            | BinOpKind.LogicalOr ->
                 fp.WriteLine "  or %rdi, %rax"
                 fp.WriteLine "  cmp $0, %rax"
                 fp.WriteLine "  setne %al"
                 fp.WriteLine "  movzb %al, %rax"
            | _ -> failwith $"unsupported binop  %A{ast}"
    | _ -> failwith $"unsupported node  %A{ast}"
    
let gen_funccall(fp, funccall: NdFuncCall) =
    // パラメータは１つのみ
    gen_expr(fp, funccall.Params.Head)
    
    //gen_exprの値はraxに入っている。
    //パラメータ一つの時はrdiに値を入れる。    
    //fp.WriteLine "  mov %rax %rdi"
    push(fp)
    pop(fp, "%rdi")
    //関数呼び出し
    fp.WriteLine $"  call %s{funccall.Name}"
    
    //戻り値は一旦０
    fp.WriteLine "  mov $0, %rax"


let gen_body(fp, asts: Ast list) =
    for ast in asts do
        match ast with
        | Ast.FuncCall(funcall) -> gen_funccall(fp, funcall)
        | _ -> failwith $"unsupported node  %A{ast}"
    

let generate(asm_file_path : string,  fn : Environment.Function)=
     let fp = new StreamWriter(asm_file_path)
     match fn.NdFunction with
     | Some(func) ->
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
     | _ -> ()
          
     fp.Close()
     
     