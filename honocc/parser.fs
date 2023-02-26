module Parser
open Honodef
open Tokenizer
open Csource

exception ParseError of Token * string


let private skip(token_stream : TokenStream, token_kind : TokenKind) =
    let token = token_stream.get()
    if token.Kind <> token_kind then
        let exp = ParseError(token, $"Parse Error expected %A{token_kind}")
        raise exp
    token_stream.consume()

// function = type identifier "(" type ")" "{" stmt "return" stmt "}"
let rec p_function (ts : TokenStream) : NdFunction =
     let token = ts.get()
     p_type ts |> ignore
     let name = identifier ts
     skip(ts, TokenKind.LParen)
     p_type ts |> ignore
     skip(ts, TokenKind.RParen)
     skip(ts, TokenKind.LBrace)
     let ast = stmt ts
     skip(ts, TokenKind.Return)
     stmt ts |> ignore
     skip(ts, TokenKind.RBrace)
     
     { Name = name; Body = [ast]; Src = token.Src }    

and identifier ts =
    let token = ts.get()
    ts.consume()
    match token.Kind with
    | TokenKind.Identifier(s) -> s
    | _ -> raise(ParseError(token, $"token must be identifier"))
    
// type = void
//      | int 
and p_type ts =
    let token = ts.get()
    ts.consume()
    match token.Kind with
    | TokenKind.Int -> TokenKind.Int
    | TokenKind.Void -> TokenKind.Void
    | _ -> raise(ParseError(token, $"token must be type"))
//stmt = expr ";"
and stmt ts =
    let ast = expr ts
    skip(ts, TokenKind.SemiColon)
    ast
// expr = term { + term　}
//        term { - term }
//       | putd
and expr ts =
    let token = ts.get()
    if token.Kind = TokenKind.DebPutd then
         ts.consume()
         putd ts
    else        
        let mutable ast = term ts
        let mutable finish = false
        
        while not finish do
            let token = ts.get()
            match token.Kind with
            | TokenKind.Operator("+") -> 
                ts.consume()
                let ast_r = term ts
                ast <- Ast.BinOp({NdBinOp.op=BinOpKind.Add; NdBinOp.l=ast; NdBinOp.r = ast_r; NdBinOp.Src=token.Src })
            | TokenKind.Operator("-") -> 
                ts.consume()
                let ast_r = term ts
                ast <- Ast.BinOp({NdBinOp.op=BinOpKind.Sub; NdBinOp.l=ast; NdBinOp.r = ast_r; NdBinOp.Src=token.Src })
            | _ ->
                finish <- true
        ast
    
// term = factor { * factor }
//        | factor { / factor }
and term ts =
    let mutable ast = factor ts
    let mutable finish = false
    
    while not finish do
        let token = ts.get()
        match token.Kind with
        | TokenKind.Operator("*") ->
            ts.consume()
            let ast_r = factor ts
            ast <- Ast.BinOp({NdBinOp.op=BinOpKind.Mult; NdBinOp.l=ast; NdBinOp.r = ast_r; NdBinOp.Src=token.Src })
        | TokenKind.Operator("/") ->
            ts.consume()
            let ast_r = factor ts
            ast <- Ast.BinOp({NdBinOp.op=BinOpKind.Div; NdBinOp.l=ast; NdBinOp.r = ast_r; NdBinOp.Src=token.Src })          
        | _ ->
            finish <- true
    ast
// factor = num
//        | ( expr )
and factor ts =
    let token = ts.get()
    ts.consume()
    match token.Kind with
    | TokenKind.Integer(n) -> Ast.Num({NdNum.Value=n; NdNum.Src=token.Src})
    | TokenKind.LParen ->
        let ast = expr ts
        skip(ts, TokenKind.RParen)
        ast
    | _ -> raise(ParseError(token, $"token must be expr"))
//一時的
//putd = "putd" "(" expr ")" 
and putd ts =
    let token = ts.get()
    skip(ts, TokenKind.LParen)    
    let parameters = expr ts
    skip(ts, TokenKind.RParen)    
    Ast.FuncCall({NdFuncCall.Name = "putd"; NdFuncCall.Params = [parameters]; NdFuncCall.Src=token.Src })

let parse token_stream  =
    try 
        p_function token_stream
    with
    | ParseError (token, s) ->
        let err_pos = Csource.findErrorPosition token
        printfn($"ERROR:%s{s}")
        printfn($"%s{err_pos}")
        
        exit 1
    