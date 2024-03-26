type command =
  | Push of int       
  | Add                   
  | Sub                   
  | Mul                   
  | Div  
  | Rem                                  
  | Swap                  
  | Pop                             


type program = int * command list

(* add here all useful functions and types  related to the AST: for instance  string_of_ functions *)

let string_of_command = function
  | Push n -> "Push " ^ string_of_int n
  | Add -> "Add"
  | Sub -> "Sub"
  | Mul -> "Mul"
  | Div -> "Div"
  | Rem -> "Rem"
  | Swap -> "Swap"
  | Pop -> "Pop"


let string_of_commands cmds = String.concat " " (List.map string_of_command cmds)

let string_of_program (args, cmds) = Printf.sprintf "%i args: %s\n" args (string_of_commands cmds)

