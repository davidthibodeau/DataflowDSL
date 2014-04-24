{
open Parser
open Lexing
open Matlab
module M = Ast
exception Error of string

type bufferstate =
| BaseLevel
| CommentLevel of int

let commentdepth = function
  | CommentLevel n -> n
  | _ -> assert false
}

let id = ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']*
let empty = [' ' '\t']
let newline = ['\n']

rule lex state = parse
| empty {lex state lexbuf}
| newline {Lexing.new_line lexbuf; lex state lexbuf}
| "Forward" {FORWARD}
| "Backward" {BACKWARD}
| "of" {OF}
| "{}" {EMPTYSET}
| "==" {EEQ}
| "+=" {PEQ}
| "-=" {MEQ}
| "*=" {TEQ}
| '=' {EQ}
| '(' {LBRACKET}
| ')' {RBRACKET}
| '{' {LCURLY}
| '}' {RCURLY}
| "analysis" {ANALYSIS}
| "merge" {MERGE}
| "flow" {FLOW}
| "at" {AT}
| "as" {AS}
| "is" {IS}
| "where" {WHERE}
| "if" {IF}
| "for" {FOR}
| "true" {TRUE}
| "false" {FALSE}
| "->" {ARR}
| '|' {VERT}
| ':' {COLON}
| ';' {SCOLON}
| '+' {PLUS}
| '-' {MINUS}
| '*' {TIMES}
| "Set" {SET}
| ',' {COMMA}
| eof {EOF}

| "/*" {state := (CommentLevel 0); comment state lexbuf } (*Comments are just ignored *)
| "//"[^ '\n']*"\n" {Lexing.new_line lexbuf; lex state lexbuf}
| id as s       
    {
     try
      MATLAB (Hashtbl.find M.keywords s) 
    with Not_found ->
      ID (s)
   }
| _ { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" 
                      (Lexing.lexeme_start lexbuf))) }

and comment state = parse
| newline {Lexing.new_line lexbuf; comment state lexbuf}
| "*/" {let n = commentdepth !state in
        if n > 0 then 
          (state := (CommentLevel (n-1)); comment state lexbuf)
        else
          (state := BaseLevel; lex state lexbuf)}
| "/*" {state := (CommentLevel (commentdepth !state + 1)); comment state lexbuf}
| _    {comment state lexbuf}
