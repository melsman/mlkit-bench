structure ForkJoin = struct

fun par (f,g) = (f(),g())
fun alloc n v = Array.tabulate(n,fn _ => v)
fun parfor X (lo, hi) (f:int->unit) : unit =
    if lo >= hi then () else (f lo; parfor X (lo+1, hi) f)

end
