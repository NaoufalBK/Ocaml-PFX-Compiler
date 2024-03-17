{
  open Parser
  open Location

  let lexbuf_position lexbuf =
    let start_pos = lexbuf.lexeme_start_p in
    let end_pos = lexbuf.lexeme_end_p in
    {
      pos_fname = start_pos.pos_fname;
      pos_lnum = start_pos.pos_lnum;
      pos_bol = start_pos.pos_bol;
      pos_cnum = end_pos.pos_cnum
    }

  let mk_int nb lexbuf =
    try
      let pos = lexbuf_position lexbuf in
      INT (int_of_string nb), pos
    with Failure _ ->
      raise (Error (Printf.sprintf "Illegal integer '%s': " nb, lexbuf_position lexbuf))
}

let newline = (['\n' '\r'] | "\r\n")
let blank = [' ' '\014' '\t' '\012']
let not_newline_char = [^ '\n' '\r']
let digit = ['0'-'9']

rule token = parse
  (* newlines *)
  | newline { incr_line lexbuf; token lexbuf }
  (* blanks *)
  | blank + { token lexbuf }
  (* end of file *)
  | eof      { EOF, Location.curr lexbuf }
  (* comments *)
  | "--" not_newline_char*  { token lexbuf }
  (* integers *)
  | digit+ as nb           { mk_int nb lexbuf }
  (* commands  *)
  | "Push"    { Push, Location.curr lexbuf }
  | "Add"     { Add, Location.curr lexbuf }
  | "Sub"     { Sub, Location.curr lexbuf }
  | "Mul"     { Mul, Location.curr lexbuf }
  | "Div"     { Div, Location.curr lexbuf }
  | "Rem"     { Rem, Location.curr lexbuf }
  | "Swap"    { Swap, Location.curr lexbuf }
  | "Pop"     { Pop, Location.curr lexbuf }
  (* illegal characters *)
  | _ as c                  { raise (Error (Printf.sprintf "Illegal character '%c': " c, lexbuf_position lexbuf)) }
