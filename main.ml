
open Parsing;;
open Lexing;;

open Lambda;;
open Parser;;
open Lexer;;

let top_level_loop () =
  print_endline "Evaluator of lambda expressions...";
  let rec loop ctx acc =
    print_string ">> ";
    flush stdout;
    try
      let line = read_line () in
      let acc' = acc ^ line ^ "\n" in
      if String.contains line ';' then
        let tm_str = String.sub acc' 0 (String.length acc' - 2) in
        let tm_str' = String.map (function '\n' -> ' ' | c -> c) tm_str in
        let tm = s token (from_string tm_str') in
        let tyTm = typeof ctx tm in
        print_endline (string_of_term (eval tm) ^ " : " ^ string_of_ty tyTm);
        loop ctx ""
      else
        loop ctx acc'
    with
      Lexical_error ->
        print_endline "lexical error";
        loop ctx ""
    | Parse_error ->
        print_endline "syntax error";
        loop ctx ""
    | Type_error e ->
        print_endline ("type error: " ^ e);
        loop ctx ""
    | End_of_file ->
        print_endline "...bye!!!"
  in
    loop emptyctx ""
;;

top_level_loop ()
;;