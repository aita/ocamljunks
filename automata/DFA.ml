open Base

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
      if List.mem automata.accept state (=) then
        Ok state
      else
        Or_error.error_string "not accepted"
    | symbol::tl ->
      match find automata.transition state symbol with
      | None -> Or_error.error_string "no transition"
      | Some next -> step next tl
  in
  step automata.initial

let to_dot ~string_of_alphabet ~string_of_state automata =
  let open Printf in
  let buf = Buffer.create 64 in
  bprintf buf "digraph G {\n";
  bprintf buf "\trankdir=LR;\n";
  bprintf buf "\tempty [label = \"\" shape = plaintext];\n";
  let accept = String.concat ~sep:" " @@ List.map automata.accept string_of_state in
  bprintf buf "\tnode [shape = doublecircle]; %s;\n" accept;
  bprintf buf "\tnode [shape = circle];\n";  
  bprintf buf "\tempty -> %s [label = \"start\"];\n" @@ string_of_state automata.initial;
  let print_graph (x, y, z) =
    let x, y, z = string_of_state x, string_of_alphabet y, string_of_state z in
    bprintf buf "\t%s -> %s [label = \"%s\"];\n" x z y
  in  
  List.iter automata.transition print_graph;
  bprintf buf "}\n";
  Buffer.contents buf
