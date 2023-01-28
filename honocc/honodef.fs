module Honodef

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
   | EOF
   
type Token = { Kind : TokenKind; Src : string; Line : int ; Pos : int }

