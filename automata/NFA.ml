open Base

type 'a symbol = Empty | Symbol of 'a

type ('a, 'b) t = {
  alphabet : 'a symbol list;
  transition : ('b * 'a symbol * 'b list) list;
  initial : 'b;
  accept : 'b list;
}
