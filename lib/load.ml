open! Parser
open Lexer 
open! Parse_ast


let report_loc file (b,e) =
  let open Lexing in 
  let open Format in 
  let l = b.pos_lnum in
  let fc = b.pos_cnum - b.pos_bol + 1 in
  let lc = e.pos_cnum - b.pos_bol + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" file l fc lc



let load file = 
  let c = open_in file in 
  let lb = Lexing.from_channel c in 
  try Parser.file Lexer.token lb 
  with 
  |Lexical_error s -> 
    report_loc file Lexing.(lexeme_start_p lb, lexeme_end_p lb);
    Format.eprintf "lexical error: %s\n@." s;
    exit 1
  |Parsing.Parse_error ->
    report_loc file Lexing.(lexeme_start_p lb, lexeme_end_p lb);
    Format.eprintf "syntax error\n@.";
    exit 1
(*   |Typing.Error(l,e) ->
    report_loc l;
    eprintf "%a\n@." Typing.report e;
    exit 1 *)
  | e ->
      Format.  eprintf "Anomaly: %s\n@." (Printexc.to_string e);
      exit 1