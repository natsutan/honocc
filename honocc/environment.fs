module Environment

open Honodef

type Function(name:string) =
    let mutable locals : Variable list = []
    let mutable nodeFunction : NdFunction option = None
    member prt.Name = name
    member prt.NdFunction
        with get() = nodeFunction
        and set(n) = nodeFunction <- n
        
    
    
    