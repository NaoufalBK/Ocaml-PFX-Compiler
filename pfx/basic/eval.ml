
open Printf

let string_of_stack stack = sprintf "[%s]" (String.concat ";" (List.map string_of_int stack))

let string_of_state (cmds,stack) =
  (match cmds with
   | [] -> "no command"
   | cmd::_ -> sprintf "executing %s" (string_of_command cmd))^
    (sprintf " with stack %s" (string_of_stack stack))

(* Question 4.2 *)
let step state =
  match state with
  | [], _ -> Error ("Nothing to step", state)
  | Push n :: q, stack -> Ok (q, n :: stack)
  | Add :: q, v2 :: v1 :: stack -> Ok (q, (v1 + v2) :: stack)
  | Sub :: q, v2 :: v1 :: stack -> Ok (q, (v1 - v2) :: stack)
  | Mul :: q, v2 :: v1 :: stack -> Ok (q, (v1 * v2) :: stack)
  | Div :: q, v2 :: v1 :: stack ->
      if v2 = 0 then
        Error ("Division by zero", state)
      else
        Ok (q, (v2 / v1) :: stack)
  | Rem :: q, v2 :: v1 :: stack ->
      if v2 = 0 then
        Error ("Modulo by zero", state)
      else
        Ok (q, (v2 mod v1) :: stack)
  | Swap :: q, v2 :: v1 :: stack -> Ok (q, v1 :: v2 :: stack)
  | Pop :: q, _ :: stack -> Ok (q, stack)
  | _  -> Error ("Invalid operation", state)

let eval_program (numargs, cmds) args =
  let rec execute = function
    | [], []    -> Ok None
    | [], v::_  -> Ok (Some v)
    | state ->
       begin
         match step state with
         | Ok s    -> execute s
         | Error e -> Error e
       end
  in
  if numargs = List.length args then
    match execute (cmds,args) with
    | Ok None -> printf "No result\n"
    | Ok(Some result) -> printf "= %i\n" result
    | Error(msg,s) -> printf "Raised error %s in state %s\n" msg (string_of_state s)
  else printf "Raised error \nMismatch between expected and actual number of args\n"
