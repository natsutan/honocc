module Riscv

open System.IO
open Honodef

let mutable stack_count = 0


let push (fp : StreamWriter, reg) =
    stack_count <- stack_count + 1
    fp.WriteLine $"# push %s{reg}"
    fp.WriteLine "  addi sp, sp, -8"
    fp.WriteLine $"  sd %s{reg}, 0(sp)"
    

let pop(fp : StreamWriter, reg) =
    stack_count <- stack_count - 1
    fp.WriteLine $"# pop %s{reg}"
    fp.WriteLine $"  ld %s{reg}, 0(sp)"
    fp.WriteLine "  addi sp, sp, 8"

let number(fp : StreamWriter, num:NdNum) =
    fp.WriteLine $"  li      a0, %d{num.Value} "

let rec gen_expr(fp, ast) =
    match ast with
    | Num(node_num) -> number(fp, node_num)
    | BinOp(binop) ->
        // Binopの右オペランドをa1に入れ、左オペランドをa0,結果をa0に入れる。
        gen_expr(fp, binop.r)
        //a0に入っている右オペランドをpush
        push(fp, "a0")
        gen_expr(fp, binop.l)
        //a1にpushした右オペランドをpop
        pop(fp, "a1")
        match binop.op with
            | BinOpKind.Add -> fp.WriteLine "  add a0, a0, a1"
            | BinOpKind.Sub -> fp.WriteLine "  sub a0, a0, a1"
            | BinOpKind.Mult -> fp.WriteLine "  mul a0, a0, a1"            
            | BinOpKind.Div -> fp.WriteLine "  div a0, a0, a1"
            | BinOpKind.Modulo -> fp.WriteLine "  rem a0, a0, a1"
            | BinOpKind.LShift -> fp.WriteLine "  sll a0, a0, a1"            
            | BinOpKind.RShift -> fp.WriteLine "  sra a0, a0, a1"
            | BinOpKind.BitAnd -> fp.WriteLine "  and a0, a0, a1"
            | BinOpKind.BitOr -> fp.WriteLine "  or a0, a0, a1"
            | BinOpKind.BitXor -> fp.WriteLine " xor a0, a0, a1"
            | BinOpKind.LogicalAnd ->
                fp.WriteLine "  snez a0, a0"
                fp.WriteLine "  snez a1, a1"
                fp.WriteLine "  and a0, a0, a1"
                fp.WriteLine "  snez a0, a0"
            | BinOpKind.LogicalOr ->
                fp.WriteLine "  or a0, a0, a1"
                fp.WriteLine "  snez a0, a0"
            | BinOpKind.Equal ->
                fp.WriteLine "  sub a0, a0, a1"
                fp.WriteLine "  seqz a0, a0"
            | BinOpKind.NotEqual ->
                fp.WriteLine "  sub a0, a0, a1"
                fp.WriteLine "  snez a0, a0"
            | BinOpKind.LesserThan -> fp.WriteLine "  slt a0, a0, a1"
            | BinOpKind.LesserEqual ->
                fp.WriteLine "  slt a0, a1, a0"
                fp.WriteLine "  seqz a0, a0"
            | BinOpKind.GreaterThan -> fp.WriteLine "  slt a0, a1, a0"
            | BinOpKind.GreaterEqual ->
                fp.WriteLine "  slt a0, a0, a1"
                fp.WriteLine "  seqz a0, a0"
            | _ -> failwith $"unsupported binop  %A{ast}"
            
    | _ -> failwith $"unsupported node  %A{ast}"


let gen_funccall(fp: StreamWriter, funccall: NdFuncCall) =
    // パラメータは１つのみ
    gen_expr(fp, funccall.Params.Head)
    
    //exprの結果がa0に入っているので、そのまま関数の引数に
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
     fp.WriteLine "  addi    sp,sp,-16"        
     fp.WriteLine "  sd      ra,8(sp)"
     fp.WriteLine "  sd      s0,0(sp)"
     fp.WriteLine "  addi    s0,sp,16"
     fp.WriteLine ""  
     
     gen_body(fp, func.Body)  |> ignore
     assert (stack_count = 0)
     
     fp.WriteLine "# Epilogue"
     
     fp.WriteLine $".L.return.%s{func.Name}:"        
     fp.WriteLine "  li      a0, 0"
     fp.WriteLine "  ld      ra,8(sp)"  
     fp.WriteLine "  ld      s0,0(sp)"  
     fp.WriteLine "  addi    sp,sp,16"  
     fp.WriteLine "  jr      ra"  
     fp.WriteLine ""  

     fp.Close()
     


