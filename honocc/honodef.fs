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
   
   
[<StructuredFormatDisplay("{Display}")>]
type Coordinate =
   { Src : string; Line : int ; Pos : int }
   member private this.Display =
      $"%s{this.Src} line:%d{this.Line}(%d{this.Pos})" 
      
type Token = { Kind : TokenKind; Src : Coordinate }


// Parser
type BinOpKind =
   | Add
   | Sub
   | Mult
   | Div
   | Equal
   | NotEqual
   | LesserThan
   | LesserEqual
   | GreaterThan
   | GreaterEqual
   | LShift
   | RShift
   | BitAnd
   | LogicalAnd
   | BitOr
   | LogicalOr
   | BitXor
   | Modulo
   
[<StructuredFormatDisplay("{Display}")>]

// Node
type NdNum =
   { Value : int; Src :Coordinate }
   member private this.Display =
      $"value = %d{this.Value} [%A{this.Src}]" 
   
type NdFuncCall = { Name :string; Params : Ast list ; Src : Coordinate }
and NdBinOp = { op : BinOpKind; l : Ast; r : Ast ; Src : Coordinate  }
and Ast =
   | Num of NdNum
   | FuncCall of NdFuncCall
   | BinOp of NdBinOp

type NdFunction = { Name : string ; Body : Ast list ; Src : Coordinate }

// Variable
type VType = INT | VOID

[<StructuredFormatDisplay("{Display}")>]
type Variable = { Name: string ; Type : VType ; Size : int  ; Local :bool ; Offset : int}

