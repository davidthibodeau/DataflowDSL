open Lexing

let run filename ast = 
  let inBuffer = open_in filename in
  let lineBuffer = Lexing.from_channel inBuffer in
  let state = ref Lexer.BaseLevel in
  let lexing =
    (fun lexbuf -> match !state with
    | Lexer.BaseLevel -> Lexer.lex state lexbuf
    | Lexer.CommentLevel _ -> Lexer.comment state lexbuf
    ) in
  try
    let _ = ast := Parser.program lexing lineBuffer in
    let _ = ast := Types.typeProgram !ast in
    Codegen.codegen !ast
  with
  | Lexer.Error msg ->
      Printf.eprintf "%s%!" msg
  | Parser.Error ->
    let pos = Lexing.lexeme_start_p (lineBuffer) in
    Printf.eprintf "At line %d and column %d: syntax error === %s.\n%!"
      pos.pos_lnum (pos.pos_cnum - pos.pos_bol) (Lexing.lexeme (lineBuffer))
  | Types.TypingError -> ()
