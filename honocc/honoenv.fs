module Honoenv

open Honodef

type Function(name:string) =
    let mutable locals : Variable list = []
    let mutable nodeFunction : NdFunction option = None
    member prt.Name = name
    member prt.NdFunction
        with get() = nodeFunction
        and set(n) = nodeFunction <- n
        
    member prt.hasVariable(name:string) =
        let sameName x =
            x.Name = name
        match List.tryFind sameName locals with
        | Some _ -> true
        | None -> false
    
    member prt.addVariable(v:Variable) =
        locals <- locals @ [v]
    
    