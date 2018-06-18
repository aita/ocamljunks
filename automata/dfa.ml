open Core

type ('a, 'b) t = {
  alphabet: 'a list;
  transition: ('b * 'a * 'b) list;
  initial: 'b;
  accept: 'b list;
}

let rec find transition state symbol = 
  match transition with
  | [] -> None
  | (x, y, z) :: _ when x = state && y = symbol -> Some z
  | _::tl -> find tl state symbol

let run automata =
  let rec step state = function
    | [] -> 
      if List.mem automata.accept state ~equal:(=) then
        Ok state
      else
        Or_error.error_string "not accepted"
    | symbol::tl ->
      match find automata.transition state symbol with
      | None -> Or_error.error_string "no transition"
      | Some next -> step next tl
  in
  step automata.initial


let alphabet = [0; 1;]
let transition = [
  (0, 0, 1);
  (0, 1, 0);
  (1, 1, 1);
  (1, 0, 0);
]
let initial = 0
let accept = [0]
let automata = { alphabet; transition; initial; accept; }

let () = 
  match run automata [1; 0; 0; 1;] with
  | Ok state -> Printf.printf "accept %d\n" state
  | Error e ->  Printf.printf "failed %s\n" (Error.to_string_hum e)
