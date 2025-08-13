(* term.sml *)

structure Term =
struct
  datatype term
    = STR of string * term list
    | INT of {i:int}
    | CON of string
    | REF of term ref
    | NON
  exception BadArg of string
end;
