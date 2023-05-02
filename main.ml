
open Parsing;;
open Lexing;;

open Lambda;;
open Parser;;
open Lexer;;

let rec read_lines acc =
  let line = read_line () in
  if String.length line > 0 && String.get line (String.length line - 1) = ';' then
    List.rev (String.sub line 0 (String.length line - 1) :: acc)
  else
    read_lines (line :: acc)

let rec loop ctx =
  print_string ">> ";
  flush stdout;
  try
    let lines = read_lines [] in
    let input = String.concat " " lines in
    let c = s token (from_string input) in 
    loop (execute ctx c)
  with
  | Lexical_error ->
    print_endline "lexical error";
    loop ctx
  | Parse_error ->
    print_endline "syntax error";
    loop ctx
  | Type_error e ->
    print_endline ("type error: " ^ e);
    loop ctx
  | End_of_file ->
    print_endline "...bye!!!"
;;

let top_level_loop () =
  print_endline "Evaluator of lambda expressions...";
  loop emptyctx;
;;

top_level_loop ();
