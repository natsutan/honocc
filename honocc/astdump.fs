module Astdump

open System.IO
open Honodef

let private write_function (ast : NdFunction, fp) =
    let name = ast.Name
    let mn_name = name.ToUpper()
    let mn_body = mn_name + "_B"
    fprintfn fp $"\t%s{mn_name}[%s{name}] -->|body| ${mn_body}[BLOCK]"
    
    0

let dump (ast, outfile :string) =
    let fp = new StreamWriter(outfile)

    fprintfn fp "```mermaid"
    fprintfn fp "graph TD"
    write_function(ast, fp) |> ignore
    fprintfn fp "```"
    
    fp.Close()    
    0