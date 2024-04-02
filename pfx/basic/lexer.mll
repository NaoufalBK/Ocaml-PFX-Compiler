{
  open Utils.Location

  type token =
    | PUSH of int | POP | ADD | SUB | MUL | DIV | REM | INT of int | EOF

  let print_token = function
    | PUSH n -> print_string ("PUSH " ^ string_of_int n)
    | POP -> print_string "POP"
    | ADD -> print_string "ADD"
    | SUB -> print_string "SUB"
    | MUL -> print_string "MUL"
    | DIV -> print_string "DIV"
    | REM -> print_string "REM"
    | EOF -> print_string "EOF"
    | INT i -> print_int i

  let mk_int loc nb =
    try INT (int_of_string nb)
    with Failure _ -> raise (Error (Printf.sprintf "Illegal integer '%s': " nb, loc))

  let mk_push loc nb =
    try PUSH (int_of_string nb)
    with Failure _ -> raise (Error (Printf.sprintf "Illegal push operation '%s': " nb, loc))
}

let newline = (['\n' '\r'] | "\r\n")
let blank = [' ' '\014' '\t' '\012']
let not_newline_char = [^ '\n' '\r']
let digit = ['0'-'9']

rule token = parse
  (* newlines *)
  | newline { token lexbuf }
  (* blanks *)
  | blank + { token lexbuf }
  (* end of file *)
  | eof      { EOF }
  (* comments *)
  | "--" not_newline_char*  { token lexbuf }
  (* integers *)
  | digit+ as nb           { mk_int (symbol_loc (Lexing.lexeme_start_p lexbuf) (Lexing.lexeme_end_p lexbuf)) nb }
  (* commands  *)
  | "push " + (digit+ as nb)  { mk_push (symbol_loc (Lexing.lexeme_start_p lexbuf) (Lexing.lexeme_end_p lexbuf)) nb  }
  | "pop"     { POP }
  | "add"     { ADD }
  | "sub"     { SUB }
  | "mul"     { MUL }
  | "div"     { DIV }
  | "rem"     { REM }
  (* illegal characters *)
  | _ as c                  { raise (Error (Printf.sprintf "Illegal character '%c': " c, symbol_loc (Lexing.lexeme_start_p lexbuf) (Lexing.lexeme_end_p lexbuf))) }

  (* {
  let rec examine_all lexbuf =
    let result = token lexbuf in
    print_token result;
    print_string " ";
    match result with
    | EOF -> ()
    | _   -> examine_all lexbuf

  let compile file =
  print_string ("File "^file^" is being treated!\n");
  try
    let input_file = open_in file in
    let lexbuf = Lexing.from_channel input_file in
    examine_all lexbuf;
    print_newline ();
    close_in (input_file)
  with Sys_error _ ->
    print_endline ("Can't find file '" ^ file ^ "'")

  let _ = Arg.parse [] compile ""
} *)
