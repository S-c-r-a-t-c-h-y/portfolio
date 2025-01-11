{
open Parser
}

let ident = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*
let float = (['0'-'9']+(['.']['0'-'9']*)?|['.']['0'-'9']+)

rule token = parse
    [' ' '\t']     { token lexbuf }     (* skip blanks *)
  | float as lxm   { FLOAT(float_of_string lxm) }
  | ident as s     { IDENT s }
  | '+'            { PLUS }
  | '-'            { MINUS }
  | '*'            { TIMES }
  | '/'            { DIV }
  | '^'            { POWER }
  | '('            { LPAREN }
  | ')'            { RPAREN }
  | eof            { EOF }