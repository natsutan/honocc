module Honodef

// Tokenizer
type TokenKind =
   | Integer of int
   | Operator of string
   | LParen
   | RParen
   | LBrace
   | RBrace
   | LBracket
   | RBracket
   | Identifier of string
   | Str of string
   | If
   | Else
   | For
   | While
   | SizeOf
   | Comma
   | SemiColon
   | Return
   | Int
   | Char
   | Void
   | DebPutd
   | EOF
   
type Coordinate =  { Src : string; Line : int ; Pos : int }

type Token = { Kind : TokenKind; Src : Coordinate }


// Parser
type BinOpKind =
   | Add
   | Sub
   | Mult
   | Div

type NdNum = { Value : int; Src :Coordinate }
type NdFuncCall = { Name :string; Params : Ast list ; Src : Coordinate }
and NdBinOp = { op : BinOpKind; l : Ast; r : Ast ; Src : Coordinate  }
and Ast =
   | Num of NdNum
   | FuncCall of NdFuncCall
   | BinOp of NdBinOp

type NdFunction = { Name : string ; Body : Ast list ; Src : Coordinate }