open Lexing

let run filename action ast = 
  let inBuffer = open_in filename in
  let lineBuffer = Lexing.from_channel inBuffer in
  let state = ref Lexer.BaseLevel in
  let lexing =
    (fun lexbuf -> match !state with
    | Lexer.BaseLevel -> Lexer.lex state lexbuf
    | Lexer.CommentLevel _ -> Lexer.comment state lexbuf
    ) in
  try
    let () = ast := Parser.program lexing lineBuffer in
    match action with
    | "parse" -> ()
    | "type" -> let _ = Types.typeProgram !ast in ()
 (*   | "gen" -> Codegen.codegen !ast *)
    | _ ->
      let _ = print_endline "Invalid action" in ()
  with
  | Lexer.Error msg ->
      Printf.eprintf "%s%!" msg
  | Parser.Error ->
let pos = Lexing.lexeme_start_p (lineBuffer) in
    Printf.eprintf "At line %d and column %d: syntax error === %s.\n%!"
      pos.pos_lnum (pos.pos_cnum - pos.pos_bol) (Lexing.lexeme (lineBuffer))
