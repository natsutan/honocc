module Astdump

open System.IO
open Honodef


let rec private write_funccall(funccall : NdFuncCall, fp, node_num) =
    let func_name = funccall.Name
    let nn_func = node_num + 1
    let mutable nn_param = nn_func + 1
    fprintfn fp $"\t%d{node_num} -->%d{nn_func}[CALL \\n %s{func_name}]"
    for p in funccall.Params do
        fprintfn fp $"\t%d{nn_func} -->|param| %d{nn_param}"    
        let nn_new = write_ast(p, fp, nn_param)             
        nn_param <- nn_new
        
    nn_param
    
and write_number(number : NdNum, fp, node_num) =
    let value = number.Value
    fprintfn fp $"\t%d{node_num}([num:%d{value}])"
    node_num + 1
    
and write_ast(ast, fp, node_num) =
    let parent = node_num

    match ast with
    | Num(num) -> write_number(num, fp, node_num)
    | FuncCall(fcall) -> write_funccall(fcall, fp, node_num)

let private write_function (ast : NdFunction, fp, node_num) =
    let name = ast.Name
    let mn_name = name.ToUpper()
    let nn_body = node_num + 1
    fprintfn fp $"\t%d{node_num}[%s{name}] -->|body| %d{nn_body+1}[BLOCK]"
    
    let mutable nn_block = nn_body + 1
    for b in ast.Body do
        let nn_new = write_ast(b, fp, nn_block)
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