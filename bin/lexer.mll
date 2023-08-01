{
  open Parser
}

let white = [' ' '\t' '\n']+
let digit = ['0'-'9']
let int = '-'? digit+
let letter = ['a'-'z' 'A'-'Z']
let id = letter+

rule read =
  parse
    | white { read lexbuf }
    | "def" { DEF }
    | "defn" { DEFN }
    | "fn" { FN }
    | "(" { LPAREN }
    | ")" { RPAREN }
    | "[" { LBRACK }
    | "]" { RBRACK }
    | int as x { INT (x |> int_of_string) }
    | id as x { IDENT x }
    | eof { EOF }


