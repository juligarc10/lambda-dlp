
{
  open Parser;;
  exception Lexical_error;; 
}

rule token = parse
    [' ' '\t']  { token lexbuf }
  | "lambda"    { LAMBDA }
  | "L"         { LAMBDA }
  | "true"      { TRUE }
  | "false"     { FALSE }
  | "if"        { IF }
  | "then"      { THEN }
  | "else"      { ELSE }
  | "succ"      { SUCC }
  | "pred"      { PRED }
  | "letrec"    { LETREC }
  | "iszero"    { ISZERO }
  | "let"       { LET }
  | "in"        { IN }
  | "Bool"      { BOOL }
  | "Nat"       { NAT }
  | '('         { LPAREN }
  | ')'         { RPAREN }
  | '.'         { DOT }
  | ','         { COMMA }
  | "++"        { PLUSPLUS }
  | '='         { EQ }
  | ':'         { COLON }
  | "->"        { ARROW }
  | ";"         { SEMICOLON }
  | "unit"      { UNIT }
  | ['0'-'9']+  { INTV (int_of_string (Lexing.lexeme lexbuf)) }
  | '"'         { read_string (Buffer.create 17) lexbuf }
  | ['a'-'z']['a'-'z' '_' '0'-'9']*
                { STRINGV (Lexing.lexeme lexbuf) }
  | eof         { EOF }
  | _           { raise Lexical_error } 

  and read_string buf = parse
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _         { raise Lexical_error }
  | eof       { raise Lexical_error }