module Honoenv

open Honodef

type Function(name:string) =
    let mutable locals : Variable list = []
    let mutable nodeFunction : NdFunction option = None
    
    //stack size
    let mutable total_stack_size = 0
    member prt.Name = name
    member prt.NdFunction
        with get() = nodeFunction
        and set(n) = nodeFunction <- n
        
    member ptr.stackSize = total_stack_size
    
    member prt.hasVariable(name:string) =
        let sameName x =
            x.Name = name
        match List.tryFind sameName locals with
        | Some _ -> true
        | None -> false
    
    member prt.addVariable(v:Variable) =
        locals <- locals @ [v]
    
    member prt.setLocalVariableOffset() =
        let mutable offset = 0
        let mutable locals_tmp = []
        let mutable stack_size = 0
        for v in locals do
            offset <- offset - v.Size
            stack_size <- stack_size + v.Size
            let new_v :Variable = {Name=v.Name ; Type=v.Type; Size =v.Size  ; Local = v.Local ; Offset = offset}
            locals_tmp <- locals_tmp @ [new_v]
        locals <- locals_tmp
        total_stack_size <- stack_size
        
    member prt.printLocals() =
        printfn $"%s{name}"
        for v in locals do
            printfn $"%s{v.Name} %A{v.Type}, offset = %d{v.Offset}"
    
    //変数名からoffsetの値を返す。なければ例外を投げる
    member prt.getOffset(name:string) =
        let sameName x =
            x.Name = name
        match List.tryFind sameName locals with
        | Some v -> v.Offset
        | None -> failwith $"%s{name} is not found"
    