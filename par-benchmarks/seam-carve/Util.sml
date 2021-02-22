(* Adapted from the MPL examples: https://github.com/MPLLang/mpl/tree/master/examples *)

structure Util:
sig
  val die: string -> 'a
  val for: (int * int) -> (int -> unit) -> unit
end =
struct
  fun die msg =
    ( TextIO.output (TextIO.stdErr, msg ^ "\n")
    ; TextIO.flushOut TextIO.stdErr
    ; OS.Process.exit OS.Process.failure
    )

  fun for (lo, hi) f =
    if lo >= hi then () else (f lo; for (lo+1, hi) f)
end
