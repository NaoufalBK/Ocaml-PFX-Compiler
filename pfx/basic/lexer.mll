{
  open Parser 
  open Utils.Location 

 
  let mk_int nb =
    try INT (int_of_string nb)
    with Failure _ -> failwith (Printf.sprintf "Illegal integer '%s': " nb)

  let print_token = function
| SWAP        -> print_string "Swap" 
| POP   -> print_string "Pop"
| PUSH i -> print_string ("Push " ^ (string_of_int i)  )
| ADD   -> print_string "Add"
| MUL  -> print_string "Mul"
| SUB-> print_string "Sub"
| DIV  -> print_string "Div"
| REM   -> print_string "Rem"
| EOF->  print_string "\n"
| _ -> failwith (Printf.sprintf "Not a token ")

 }

let newline = (['\n' '\r'] | "\r\n")
let blank = [' ' '\014' '\t' '\012']
let not_newline_char = [^ '\n' '\r']
let integer = ['0'-'9']+

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

  | integer as nb       {  (mk_int nb ) }
  (***** TO COMPLETE *****)
  (* commands  *)
  | "Swap" { SWAP }
  | "Sub" { SUB }
  | "Div" { DIV }
  | "Mul" { MUL }
  | "Rem" { REM }

  (* illegal characters *)  | "Add" { ADD }

  | _ as c                  { failwith (Printf.sprintf "\nIllegal character '%c' \n error located at '%s'" c ( string_of (curr lexbuf) ) )  }

  
  | "Pop" { POP }
  | "Push" blank+ (integer+ as nb )  { PUSH ( int_of_string nb)}