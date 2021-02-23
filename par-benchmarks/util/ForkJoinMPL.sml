structure ForkJoin : FORK_JOIN = struct

(* MPL's builtin ForkJoin. *)
structure FJ = ForkJoin

val par = FJ.par
val parfor = FJ.parfor
val pmap = List.map

fun alloc n _ = Array.alloc n
fun pair (f,g) (x,y) = par (fn () => f x, fn () => g y)

type gcs = int * int
fun parfor' ((_,g): gcs) = parfor g
end
