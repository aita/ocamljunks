open Base
open Automata

let alphabet = [0; 1;]
let transition = [
  (0, 0, 1);
  (0, 1, 0);
  (1, 1, 1);
  (1, 0, 0);
]
let initial = 0
let accept = [0]
let automata = { DFA.alphabet; transition; initial; accept; }

let () = 
  Stdio.print_endline @@ DFA.to_dot Int.to_string Int.to_string automata 

(* 
let () =
  match DFA.run automata [1; 0; 0; 1;] with
  | Ok state -> Stdio.printf "accept %d\n" state
  | Error e ->  Stdio.printf "failed %s\n" (Error.to_string_hum e) 
  *)
