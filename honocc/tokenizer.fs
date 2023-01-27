module Tokenizer
open System.IO
open Honodef

let private createToken (kind :TokenKind,  src :string,line :int ,pos :int) : Token =
    {Kind = kind; Src = src; Line = line; Pos = pos}
    
let private printToken (token : Token) =
    printfn $"%A{token.Kind}"
     
let debPrintTokens (tokens: Token List) =
    printfn "===== TOKENIZE ====="
    List.map printToken tokens
     

let tokenizeFromFile (filename  :string) =
    printfn $"Open %s{filename}"
   
    let input_str =
        try
            File.ReadAllLines filename
        with
            | :? FileNotFoundException -> failwith $"Can not open %s{filename} "
            
            
    printfn $"%A{input_str}"
    
    
    let mutable tokens = []
    let token0 = createToken(TokenKind.EOF, filename, 0, 0)
    tokens <- tokens @ [token0]
    let token1 = createToken(TokenKind.Integer(5), filename, 0, 0)
    tokens <- tokens @ [token1]
    let token2 = createToken(TokenKind.LParen, filename, 0, 0)
    tokens <- tokens @ [token2]
    
    
    tokens    
     