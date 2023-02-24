module Csource
open System.IO
open System
open Honodef

let private indicate p =
    let mutable s = ""
    for _ in 0..p-1 do
        s <- s + " "
    s <- s + "^"
    s

let findErrorPosition(token :Token) : String =
    let filename = token.Src.Src
    let line_num = token.Src.Line
    let input_lines = File.ReadAllLines(filename)
    let line = input_lines.[line_num - 1]
    let pos = token.Src.Pos
    let ind = indicate pos
    
    $"%s{filename} Line: %d{line_num}\n%s{line}\n%s{ind}"
