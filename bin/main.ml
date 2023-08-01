open Types

let parse s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let rec interp t_ctx =
  try
    print_string ">> ";
    let line = read_line () |> String.trim in
    let _, typ, res_t_ctx = line |> parse |> Types.typechecker t_ctx in
    "- : " ^ string_of_typ typ |> print_endline;
    interp res_t_ctx
  with End_of_file -> exit 0

let () = interp SMap.empty
