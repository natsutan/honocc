module Tokenizer
open System.IO
open System
open Honodef

let private createCoordinate ( src : string,  line : int , pos : int ) : Coordinate =
    { Src = src; Line = line ; Pos = pos }

let private createToken (kind :TokenKind, src :string,line :int ,pos :int) : Token =
    {Kind = kind; Src = createCoordinate(src, line, pos)}
    
let private printToken (token : Token) =
    printfn $"%A{token.Kind} line:%A{token.Src.Line}"
     
let private isDigit c =
    match c with
       | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> true
       | _ -> false
     
let private isAlpha c =
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

let private printTokens (tokens: Token List) =
    printfn "===== TOKENIZE ====="
    List.map printToken tokens |> ignore
    () 
     
let rec private tokenize (input_str :string, filename : string, line : int, pos : int) =
    if input_str = "" then
        [createToken(TokenKind.EOF, filename, line, pos)]
    else
        let c = input_str[0]
        match c with
          | ' ' -> tokenize(input_str[1..], filename, line, pos + 1)
          | '\n' -> tokenize(input_str[1..], filename, line + 1, 0)
          | '('  -> createToken(TokenKind.LParen, filename, line, pos) :: tokenize(input_str[1..], filename, line, pos + 1)
          | ')'  -> createToken(TokenKind.RParen, filename, line, pos) :: tokenize(input_str[1..], filename, line, pos + 1)
          | '{'  -> createToken(TokenKind.LBrace, filename, line, pos) :: tokenize(input_str[1..], filename, line, pos + 1)
          | '}'  -> createToken(TokenKind.RBrace, filename, line, pos) :: tokenize(input_str[1..], filename, line, pos + 1)
          | ';'  -> createToken(TokenKind.SemiColon, filename, line, pos) :: tokenize(input_str[1..], filename, line, pos + 1)
          | '+'  -> createToken(TokenKind.Operator("+"), filename, line, pos) :: tokenize(input_str[1..], filename, line, pos + 1)
          | '-'  -> createToken(TokenKind.Operator("-"), filename, line, pos) :: tokenize(input_str[1..], filename, line, pos + 1)
          | '*'  -> createToken(TokenKind.Operator("*"), filename, line, pos) :: tokenize(input_str[1..], filename, line, pos + 1)
          | '/'  -> createToken(TokenKind.Operator("/"), filename, line, pos) :: tokenize(input_str[1..], filename, line, pos + 1)          
          | d when isDigit d ->
              let mutable p = 1
              while isDigit input_str.[p] do
                  p <- p + 1
              let value = input_str[..p-1] |> int
              let token = createToken(TokenKind.Integer(value), filename, line, pos)
              token :: tokenize(input_str[p..], filename, line, pos + p)
          | a when isAlpha a ->
              let mutable p = 1
              while isAlpha input_str.[p] do
                  p <- p + 1
              let s = input_str[..p-1] |> string
              let token =
                match s with
                | "return" -> createToken(TokenKind.Return, filename, line, pos)
                | "int" -> createToken(TokenKind.Int, filename, line, pos)
                | "void" -> createToken(TokenKind.Void, filename, line, pos)
                | "putd" -> createToken(TokenKind.DebPutd, filename, line, pos)
                | _ -> createToken(TokenKind.Identifier(s), filename, line, pos)
              token :: tokenize(input_str[p..], filename, line, pos + p)
          |  _ -> failwith $"unsupported char  %s{input_str}"
    

type TokenStream(tokens_in : Token list) = 
    let tokens = tokens_in
    let mutable p = 0
    member this.get() =
        tokens.[p]
    member this.consume() =
        p <- p + 1
    member this.debPrintTokens() =
        printTokens tokens
    

let tokenizeFromFile (filename  :string) =
    printfn $"Open %s{filename}"
   
    let input_str =
        try
            File.ReadAllText  filename
        with
            | :? FileNotFoundException -> failwith $"Can not open %s{filename} "
            
    let tokens = tokenize(input_str, filename, 1, 0)
    TokenStream(tokens)
     