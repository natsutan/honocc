module Parser
open System
open Honodef
open Tokenizer
open Csource

exception ParseError of Token * string

let mutable locals : Variable list = []

let private skip(token_stream : TokenStream, token_kind : TokenKind) =
    let token = token_stream.get()
    if token.Kind <> token_kind then
        let exp = ParseError(token, $"Parse Error expected %A{token_kind}")
        raise exp
    token_stream.consume()

let is_definition(ts : TokenStream) : bool =
    let rec ck_definition (ts : TokenStream, i : int) : bool =
        let token = ts.peek(i)
        match token.Kind with
        | TokenKind.SemiColon -> true
        | TokenKind.Void | TokenKind.Int | TokenKind.Identifier(_)  -> ck_definition(ts, i+1)
        | _ -> false
                
    let token = ts.get()

    match token.Kind with
    | TokenKind.Void | TokenKind.Int ->
        ck_definition(ts, 1)            
    | _ -> false

let is_type(token: Token) : bool =
    match token.Kind with
    | TokenKind.Void | TokenKind.Int -> true
    | _ -> false

let add_variable (ts : TokenStream, fn : Honoenv.Function, vtype: VType) : Variable  =
    let token = ts.get()
    match token.Kind with
    | TokenKind.Identifier(name) ->
        if fn.hasVariable(name) then
            let exp = ParseError(token, $"Parse Error variable %s{name} already defined ")
            raise exp
        ts.consume()
        Ast.Variable({NdVariable.Name=name; NdVariable.Src=token.Src}) |> ignore
        {Name = name; Type = vtype; Size = 8; Local = true ; Offset = 0}
    | _ -> raise(ParseError(token, $"token must be variable"))

// definition = type identifier
let definition(ts : TokenStream, fn : Honoenv.Function)=
    // 変数の型
    let token_t = ts.get()
    let vtype = match token_t.Kind with
                | TokenKind.Int -> VType.INT
                | _ ->
                    let exp = ParseError(token_t, $"Parse Error token must be int ")
                    raise exp
    ts.consume()
    
    //　変数名
    let token_n = ts.get()
    
    match token_n.Kind with
    | TokenKind.Identifier(name) ->
        let variable  = add_variable(ts, fn, vtype)
        fn.addVariable(variable)
    | _ ->
        let exp = ParseError(token_n, $"Parse Error token must be variable name ")
        raise exp
    
    

// function = type identifier "(" type ")" "{" stmt "return" stmt "}"
let rec p_function (ts : TokenStream) : Honoenv.Function =
     let mutable body : Ast list = []
     let mutable token = ts.get()
     let mutable fn = Honoenv.Function("main") 
     
     p_type (ts, fn) |> ignore
     let name = identifier(ts, fn)
     skip(ts, TokenKind.LParen)
     p_type (ts, fn) |> ignore
     skip(ts, TokenKind.RParen)
     skip(ts, TokenKind.LBrace)
     
     token <- ts.get()
     while token.Kind <> TokenKind.Return do
        let ast = stmt (ts, fn)
        body <- body @ [ast]
        token <- ts.get()
        if token.Kind = TokenKind.EOF then
            let exp = ParseError(token, $"Parse Error unexpected EOF")
            raise exp
     
     skip(ts, TokenKind.Return)
     stmt(ts, fn) |> ignore
     skip(ts, TokenKind.RBrace)
     
     fn.NdFunction <- Some({ Name = name; Body = body; Src = token.Src })
     fn

and identifier(ts, fn) =
    let token = ts.get()
    ts.consume()
    match token.Kind with
    | TokenKind.Identifier(s) -> s
    | _ -> raise(ParseError(token, $"token must be identifier"))
    
// type = void
//      | int 
and p_type(ts, fn) =
    let token = ts.get()
    ts.consume()
    match token.Kind with
    | TokenKind.Int -> TokenKind.Int
    | TokenKind.Void -> TokenKind.Void
    | _ -> raise(ParseError(token, $"token must be type"))
//stmt = expr ";"
//     = type identifier = expr ";"
and stmt(ts, fn) =
    // 変数宣言のみの時はASTを作らない
    while is_definition ts do
        definition(ts, fn)
        skip(ts, TokenKind.SemiColon)
        
    let token = ts.get()
    if is_type token then
        //宣言と同時に代入パターン
        def_assign_stmt(ts, fn)
    else
        //exer
        stmt2(ts, fn)
and def_assign_stmt(ts, fn) =
    // 変数の型  
    let token_t = ts.get()  
    let vtype = match token_t.Kind with  
                | TokenKind.Int -> VType.INT  
                | _ ->  
                    let exp = ParseError(token_t, $"Parse Error token must be int ")  
                    raise exp  
    ts.consume()  
      
    //　変数名
    let token_n = ts.get()
    let variable : Variable = add_variable(ts, fn, vtype)
    fn.addVariable(variable)  
    skip(ts, TokenKind.Operator("="))
    let ast = expr(ts, fn)    
    let ast_binop = Ast.BinOp({NdBinOp.op=BinOpKind.Assign; NdBinOp.l=Ast.Variable({NdVariable.Name=variable.Name; NdVariable.Src=token_n.Src}); NdBinOp.r = ast; NdBinOp.Src=token_n.Src })
    skip(ts, TokenKind.SemiColon)
    ast_binop
    
and stmt2(ts, fn) =
    let ast = expr(ts, fn)
    skip(ts, TokenKind.SemiColon)
    ast
and declaration(ts, fn) =
    ()
    
// expr = assign
//       | putd
and expr (ts, fn) =
    let token = ts.get()
    match token.Kind with
    | TokenKind.DebPutd ->
         ts.consume()
         putd (ts, fn)
    | _ ->
        assign(ts, fn)

// assign = variavle { =  locicalor　}
//          | locicalor
and assign (ts, fn) =
    let token = ts.get()
    let mutable finish = false
    match token.Kind with
    | TokenKind.Identifier(name) ->
        let mutable ast = factor(ts, fn)
        while not finish do
            let token = ts.get()
            match token.Kind with
            | TokenKind.Operator("=") -> 
                ts.consume()
                let ast_r = logicalor(ts, fn)
                ast <- Ast.BinOp({NdBinOp.op=BinOpKind.Assign; NdBinOp.l=ast; NdBinOp.r = ast_r; NdBinOp.Src=token.Src })
            | _ ->
                finish <- true
        ast
        
    | _ ->  logicalor(ts, fn)
    
// logicalor = logicaland { ||  logicaland　}

and logicalor(ts, fn) = 
    let mutable ast = logicaland(ts, fn)
    let mutable finish = false
    
    while not finish do
        let token = ts.get()
        match token.Kind with
        | TokenKind.Operator("||") -> 
            ts.consume()
            let ast_r = logicaland(ts, fn)
            ast <- Ast.BinOp({NdBinOp.op=BinOpKind.LogicalOr; NdBinOp.l=ast; NdBinOp.r = ast_r; NdBinOp.Src=token.Src })
        | _ ->
            finish <- true
    ast

// logicaland = bitwiseor { && bitwiseor　}
and logicaland(ts, fn) =
    let mutable ast = bitwiseor(ts, fn)
    let mutable finish = false
    
    while not finish do
        let token = ts.get()
        match token.Kind with
        | TokenKind.Operator("&&") -> 
            ts.consume()
            let ast_r = bitwiseor(ts, fn)
            ast <- Ast.BinOp({NdBinOp.op=BinOpKind.LogicalAnd; NdBinOp.l=ast; NdBinOp.r = ast_r; NdBinOp.Src=token.Src })
        | _ ->
            finish <- true
    ast
// bitwiseor = bitwisexor { | bitwisexor　}

and bitwiseor(ts, fn) =
    let mutable ast = bitwisexor (ts, fn)
    let mutable finish = false
    
    while not finish do
        let token = ts.get()
        match token.Kind with
        | TokenKind.Operator("|") -> 
            ts.consume()
            let ast_r = bitwisexor(ts, fn)
            ast <- Ast.BinOp({NdBinOp.op=BinOpKind.BitOr; NdBinOp.l=ast; NdBinOp.r = ast_r; NdBinOp.Src=token.Src })
        | _ ->
            finish <- true
    ast
// bitwisexor = bitwiseand { ^ bitwiseand　}

and bitwisexor(ts, fn) =
    let mutable ast = bitwiseand(ts, fn)
    let mutable finish = false
    
    while not finish do
        let token = ts.get()
        match token.Kind with
        | TokenKind.Operator("^") -> 
            ts.consume()
            let ast_r = bitwiseand(ts, fn)
            ast <- Ast.BinOp({NdBinOp.op=BinOpKind.BitXor; NdBinOp.l=ast; NdBinOp.r = ast_r; NdBinOp.Src=token.Src })
        | _ ->
            finish <- true
    ast
// bitwiseand = equality { & equality　}

and bitwiseand (ts, fn) =
    let mutable ast = equality (ts, fn)
    let mutable finish = false
    
    while not finish do
        let token = ts.get()
        match token.Kind with
        | TokenKind.Operator("&") -> 
            ts.consume()
            let ast_r = equality(ts, fn)
            ast <- Ast.BinOp({NdBinOp.op=BinOpKind.BitAnd; NdBinOp.l=ast; NdBinOp.r = ast_r; NdBinOp.Src=token.Src })
        | _ ->
            finish <- true
    ast
// equality = relational { == relational　}
//            relational { != relational }

and equality(ts, fn) =
    let mutable ast = relational(ts, fn)
    let mutable finish = false
    
    while not finish do
        let token = ts.get()
        match token.Kind with
        | TokenKind.Operator("==") -> 
            ts.consume()
            let ast_r = relational(ts, fn)
            ast <- Ast.BinOp({NdBinOp.op=BinOpKind.Equal; NdBinOp.l=ast; NdBinOp.r = ast_r; NdBinOp.Src=token.Src })
        | TokenKind.Operator("!=") -> 
            ts.consume()
            let ast_r = relational(ts, fn)
            ast <- Ast.BinOp({NdBinOp.op=BinOpKind.NotEqual; NdBinOp.l=ast; NdBinOp.r = ast_r; NdBinOp.Src=token.Src })
        | _ ->
            finish <- true
    ast
// relational = shifting { < shifting　}
//              shifting { > shifting }
//              shifting { >= shifting }
//              shifting { <= shifting }

and relational(ts, fn) =
    let mutable ast = shifting(ts, fn)
    let mutable finish = false
    
    while not finish do
        let token = ts.get()
        match token.Kind with
        | TokenKind.Operator("<") -> 
            ts.consume()
            let ast_r = shifting (ts, fn)
            ast <- Ast.BinOp({NdBinOp.op=BinOpKind.LesserThan; NdBinOp.l=ast; NdBinOp.r = ast_r; NdBinOp.Src=token.Src })
        | TokenKind.Operator("<=") -> 
            ts.consume()
            let ast_r = shifting (ts, fn)
            ast <- Ast.BinOp({NdBinOp.op=BinOpKind.LesserEqual; NdBinOp.l=ast; NdBinOp.r = ast_r; NdBinOp.Src=token.Src })
        | TokenKind.Operator(">") -> 
            ts.consume()
            let ast_r = shifting (ts, fn)
            ast <- Ast.BinOp({NdBinOp.op=BinOpKind.GreaterThan; NdBinOp.l=ast; NdBinOp.r = ast_r; NdBinOp.Src=token.Src })
        | TokenKind.Operator(">=") -> 
            ts.consume()
            let ast_r = shifting (ts, fn)
            ast <- Ast.BinOp({NdBinOp.op=BinOpKind.GreaterEqual; NdBinOp.l=ast; NdBinOp.r = ast_r; NdBinOp.Src=token.Src })
        | _ ->
            finish <- true
    ast
// shifting = additive { << additive　}
//            additive { >> additive }

and shifting(ts, fn) =
    let mutable ast = additive(ts, fn)
    let mutable finish = false
    
    while not finish do
        let token = ts.get()
        match token.Kind with
        | TokenKind.Operator("<<") -> 
                ts.consume()
                let ast_r = additive(ts, fn)
                ast <- Ast.BinOp({NdBinOp.op=BinOpKind.LShift; NdBinOp.l=ast; NdBinOp.r = ast_r; NdBinOp.Src=token.Src })
        | TokenKind.Operator(">>") -> 
                ts.consume()
                let ast_r = additive (ts, fn)
                ast <- Ast.BinOp({NdBinOp.op=BinOpKind.RShift; NdBinOp.l=ast; NdBinOp.r = ast_r; NdBinOp.Src=token.Src })
        | _ ->
            finish <- true
    ast

and additive(ts, fn) =
    let mutable ast = term(ts, fn)
    let mutable finish = false
    
    while not finish do
        let token = ts.get()
        match token.Kind with
        | TokenKind.Operator("+") -> 
            ts.consume()
            let ast_r = term(ts, fn)
            ast <- Ast.BinOp({NdBinOp.op=BinOpKind.Add; NdBinOp.l=ast; NdBinOp.r = ast_r; NdBinOp.Src=token.Src })
        | TokenKind.Operator("-") -> 
            ts.consume()
            let ast_r = term(ts, fn)
            ast <- Ast.BinOp({NdBinOp.op=BinOpKind.Sub; NdBinOp.l=ast; NdBinOp.r = ast_r; NdBinOp.Src=token.Src })
        | _ ->
            finish <- true
    ast
        
// term = unary { * unary }
//        | unary { / unary }
and term(ts, fn) =
    let mutable ast = unary(ts, fn)
    let mutable finish = false
    
    while not finish do
        let token = ts.get()
        match token.Kind with
        | TokenKind.Operator("*") ->
            ts.consume()
            let ast_r = unary(ts, fn)
            ast <- Ast.BinOp({NdBinOp.op=BinOpKind.Mult; NdBinOp.l=ast; NdBinOp.r = ast_r; NdBinOp.Src=token.Src })
        | TokenKind.Operator("/") ->
            ts.consume()
            let ast_r = unary(ts, fn)
            ast <- Ast.BinOp({NdBinOp.op=BinOpKind.Div; NdBinOp.l=ast; NdBinOp.r = ast_r; NdBinOp.Src=token.Src })
        | TokenKind.Operator("%") -> 
            ts.consume()
            let ast_r = unary(ts, fn)
            ast <- Ast.BinOp({NdBinOp.op=BinOpKind.Modulo; NdBinOp.l=ast; NdBinOp.r = ast_r; NdBinOp.Src=token.Src })                
        | _ ->
            finish <- true
    ast
//
// unary   = ("+" | "-")? factor
and unary(ts, fn) =
    let token = ts.get()
    match token.Kind with
        | TokenKind.Operator("+") ->
            ts.consume()
            unary(ts, fn)
        | TokenKind.Operator("-") ->
            ts.consume()
            let node_r = unary(ts, fn)
            let node_l = Ast.Num({NdNum.Value=0; NdNum.Src=token.Src})
            Ast.BinOp({NdBinOp.op=BinOpKind.Sub; NdBinOp.l=node_l; NdBinOp.r = node_r; NdBinOp.Src=token.Src })          
        | _ ->
            factor(ts, fn)
    
// factor = num
//        | variable
//        | ( expr )
and factor(ts, fn) =
    let token = ts.get()
    ts.consume()
    match token.Kind with
    | TokenKind.Integer(n) -> Ast.Num({NdNum.Value=n; NdNum.Src=token.Src})
    | TokenKind.Identifier(name) ->
        match fn.hasVariable(name) with
        | true -> Ast.Variable({NdVariable.Name=name; NdVariable.Src=token.Src})
        | false -> raise(ParseError(token, $"unknown variable %s{name}"))
            
    | TokenKind.LParen ->
        let ast = expr(ts, fn)
        skip(ts, TokenKind.RParen)
        ast
    | _ -> raise(ParseError(token, $"token must be expr"))
//一時的
//putd = "putd" "(" expr ")" 
and putd(ts, fn) =
    let token = ts.get()
    skip(ts, TokenKind.LParen)    
    let parameters = expr(ts, fn)
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
    