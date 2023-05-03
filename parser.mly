
%{
  open Lambda;;
%}

%token LAMBDA
%token TRUE
%token FALSE
%token IF
%token THEN
%token ELSE
%token SUCC
%token PRED
%token ISZERO
%token LET
%token IN
%token BOOL
%token NAT

%token LBRAKET
%token RBRAKET
%token LPAREN
%token RPAREN
%token DOT
%token EQ
%token COLON
%token COMMA
%token ARROW
%token EOF
%token PLUSPLUS
%token LETREC
%token SEMICOLON
%token UNIT

%token TUPLE
%token PROJ

%token <int> INTV
%token <string> STRINGV
%token <string> STRING

%start s
%type <Lambda.command> s

%%

s :
    STRINGV EQ termSeq EOF
			{ Bind ($1, $3) }
  |  termSeq EOF
      { Eval $1 }

termSeq :
		term
			{ $1 }
	| termSeq SEMICOLON term
			{ TmApp ( TmAbs( "_", TyUnit, $3), $1)}

term :
    appTerm
      { $1 }
  | IF term THEN term ELSE term
      { TmIf ($2, $4, $6) }
  | LAMBDA STRINGV COLON ty DOT term
      { TmAbs ($2, $4, $6) }
  | LET STRINGV EQ term IN term
      { TmLetIn ($2, $4, $6) }
  | term PLUSPLUS term
			{ TmConcat ($1, $3) }
  | term PLUSPLUS term
        { TmConcat ($1, $3) }
  | LETREC STRINGV COLON ty EQ term IN term
      { TmLetIn ($2, TmFix( TmAbs ($2, $4, $6)), $8)}
  | LPAREN termList RPAREN
      { TmTuple($2) }
  | term DOT INTV
      { TmProj ($1, $3) }
  | LBRAKET termList RBRAKET
      { TmList($2) }


termList :
      term
        { [$1] }
    | term SEMICOLON termList
        { $1 :: $3 }
    | term COMMA termList
        { $1 :: $3 }


appTerm :
    atomicTerm
      { $1 }
  | SUCC atomicTerm
      { TmSucc $2 }
  | PRED atomicTerm
      { TmPred $2 }
  | ISZERO atomicTerm
      { TmIsZero $2 }
  | appTerm atomicTerm
      { TmApp ($1, $2) }

atomicTerm :
    LPAREN term RPAREN
      { $2 }
  | TRUE
      { TmTrue }
  | FALSE
      { TmFalse }
  | STRINGV
      { TmVar $1 }
  | STRING
      { TmString $1 }
  | INTV
      { let rec f = function
            0 -> TmZero
          | n -> TmSucc (f (n-1))
        in f $1 }
  | UNIT
	  { TmUnit }

ty :
    atomicTy
      { $1 }
  | atomicTy ARROW ty
      { TyArr ($1, $3) }
  | LPAREN tyList RPAREN
      { TyTuple($2) }
  | LBRAKET tyList RBRAKET
      { TyList ($2) }
      
  | atomicTy DOT INTV
      { TyProj ($3, $1) }


tyList :
    ty
      { [$1] }
  | ty SEMICOLON tyList
      { $1 :: $3 }
  | ty COMMA tyList
      { $1 :: $3 }


atomicTy :
    LPAREN ty RPAREN  
      { $2 } 
  | BOOL
      { TyBool }
  | NAT
      { TyNat }
  | STRING
      { TyString }
  | UNIT
	  { TyUnit }

