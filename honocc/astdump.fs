module Astdump

open System.IO
open Honodef


let rec private write_funccall(funccall : NdFuncCall, fp, node_num, parent) =
    let func_name = funccall.Name
    let nn_func = node_num + 1
    let mutable nn_param = nn_func + 1
    fprintfn fp $"\t%d{parent} -->%d{nn_func}[CALL \\n %s{func_name}]"
    for p in funccall.Params do
//        fprintfn fp $"\t%d{nn_func} -->|param| %d{nn_param}"    
        let nn_new = write_ast(p, fp, nn_param, nn_func, "|param|")             
        nn_param <- nn_new
    nn_param

and write_binop(binop: NdBinOp, fp, node_num, parent, label) =
    let op_name = match binop.op with
                    | BinOpKind.Add -> "ADD"
                    | BinOpKind.Sub -> "SUB"
                    | BinOpKind.Mult -> "MUL"
                    | BinOpKind.Div -> "DIV"
                    | BinOpKind.Equal -> "=="
                    | BinOpKind.NotEqual -> "!="
                    | BinOpKind.LesserThan -> "<"
                    | BinOpKind.LesserEqual -> "<="
                    | BinOpKind.GreaterThan -> ">"
                    | BinOpKind.GreaterEqual -> ">="
                    | BinOpKind.LShift -> "<<"
                    | BinOpKind.RShift -> ">>"
                    | BinOpKind.BitAnd -> "AND"
                    | BinOpKind.LogicalAnd -> "LAND"
                    | BinOpKind.BitOr -> "OR"
                    | BinOpKind.LogicalOr -> "LOR"
                    | BinOpKind.BitXor -> "XOR"
                    | BinOpKind.Modulo -> "Modulo"
                    | BinOpKind.Assign -> "="

    let nn_op = node_num
    let nn_left = nn_op + 1
    let nn_right = write_ast(binop.l, fp, nn_left, nn_op, "")
    let nn_next = write_ast(binop.r, fp, nn_right, nn_op, "")
    fprintfn fp $"\t%d{parent} -->%s{label}%d{nn_op}"
    fprintfn fp $"\t%d{nn_op}([%s{op_name}])"
    //fprintfn fp $"\t%d{nn_op} --> %d{nn_left}"    
    //fprintfn fp $"\t%d{nn_op} --> %d{nn_right}"    
    
    nn_next
and write_number(number : NdNum, fp, node_num, parent, label) =
    let value = number.Value
    fprintfn fp $"\t%d{node_num}([num:%d{value}])"
    fprintfn fp $"\t%d{parent} -->%s{label}%d{node_num}"
    node_num + 1
and write_variable(v: NdVariable, fp, node_num, parent, label) =
    let name = v.Name
    fprintfn fp $"\t%d{node_num}([variable:%s{name}])"
    fprintfn fp $"\t%d{parent} -->%s{label}%d{node_num}"
    node_num + 1
    
and write_ast(ast, fp, node_num, parent, label) =
    match ast with
    | Num(num) -> write_number(num, fp, node_num, parent, label)
    | Variable(v) -> write_variable(v, fp, node_num, parent, label)
    | FuncCall(fcall) -> write_funccall(fcall, fp, node_num, parent)
    | BinOp(binop) -> write_binop(binop, fp, node_num, parent, label)

let private write_function (ast : NdFunction, fp, node_num) =
    let name = ast.Name
    let mn_name = name.ToUpper()
    let nn_body = node_num + 1
    fprintfn fp $"\t%d{node_num}[%s{name}] -->|body| %d{nn_body}[BLOCK]"
    
    let mutable nn_block = nn_body
    for b in ast.Body do
        let nn_new = write_ast(b, fp, nn_block+1, nn_body, "")
        nn_block <- nn_new
    
    nn_block
    

let dump (ast, outfile :string) =
    let fp = new StreamWriter(outfile)
    let node_num = 0
    
    fprintfn fp "```mermaid"
    fprintfn fp "graph TD"
    write_function(ast, fp, node_num) |> ignore
    fprintfn fp "```"
    
    fp.Close()    
    0