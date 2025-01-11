%{
  open Expr
  open Func
%}

%token <float> FLOAT
%token <string> IDENT
%token PLUS MINUS TIMES DIV POWER
%token LPAREN RPAREN
%token EOF

%left PLUS MINUS        
%left TIMES DIV LPAREN RPAREN
%nonassoc UMINUS        
%right POWER

%start expr 
%start preprocess
%type <Expr.t> expr
%type <string> preprocess

%%

preprocess:
  | FLOAT                                       { string_of_float $1 }
  | IDENT                                       { $1 }

  // implicit multiplication to the left of parentheses
  | IDENT LPAREN preprocess                     { if is_function $1 then $1 ^ "(" ^ $3 else $1 ^ "*(" ^ $3 }
  | FLOAT LPAREN preprocess                     { (string_of_float $1) ^ "*(" ^ $3 }
  
  // implicit multiplication to the right of parentheses
  | preprocess RPAREN IDENT LPAREN preprocess   { if is_function $3 then $1 ^ ")*" ^ $3 ^ "(" ^ $5 else $1 ^ ")*" ^ $3 ^ "*(" ^ $5 }
  | preprocess RPAREN IDENT                     { $1 ^ ")*" ^ $3 }
  | preprocess RPAREN FLOAT                     { $1 ^ ")*" ^ (string_of_float $3) }

  // implicit multiplications between parentheses
  | RPAREN LPAREN                               { ")*(" }
  | preprocess RPAREN LPAREN                    { $1 ^ ")*(" }
  | RPAREN LPAREN preprocess                    { ")*(" ^ $3 }
  | preprocess RPAREN LPAREN preprocess         { $1 ^ ")*(" ^ $4 }

  // rogue parentheses
  | LPAREN preprocess                           { "(" ^ $2 }
  | preprocess RPAREN                           { $1 ^ ")" }

  // implicit multiplications with floats
  | FLOAT IDENT LPAREN preprocess               { if is_function $2 then (string_of_float $1) ^ "*" ^ $2 ^ "(" ^ $4 else (string_of_float $1) ^ "*" ^ $2 ^ "*(" ^ $4 }
  | FLOAT IDENT                                 { (string_of_float $1) ^ "*" ^ $2 }

  | preprocess TIMES preprocess                 { $1 ^ "*" ^ $3 }
  | preprocess PLUS preprocess                  { $1 ^ "+" ^ $3 }
  | preprocess MINUS preprocess                 { $1 ^ "-" ^ $3 }
  | preprocess DIV preprocess                   { $1 ^ "/" ^ $3 }
  | preprocess POWER preprocess                 { $1 ^ "^" ^ $3 }
  | MINUS preprocess %prec UMINUS               { "-" ^ $2 }

expr:
  | expr_standard { normalize $1 }

expr_standard:
  | FLOAT                                      { Float ($1) }
  | IDENT                                      { if is_function $1 then raise Parse_error else Var $1 }
  | IDENT LPAREN expr_standard RPAREN          { if $1 <> "sqrt" then Func ($1, $3) else Power ($3, Float 0.5) }
  | LPAREN expr_standard RPAREN                { $2 }
  | expr_standard TIMES expr_standard          { Times [$1; $3] }
  | expr_standard PLUS expr_standard           { Plus [$1; $3] }
  | expr_standard MINUS expr_standard          { Plus [$1; Times [Float (-1.); $3]] }
  | expr_standard DIV expr_standard            { Frac ($1, $3) }
  | expr_standard POWER expr_standard          { Power ($1, $3) }
  | MINUS expr_standard %prec UMINUS           { Times [Float (-1.); $2] }

%%