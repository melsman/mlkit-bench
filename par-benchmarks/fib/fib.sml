(* Benchmark functionality copied from MaPLe (MPL)
   https://github.com/MPLLang/mpl/blob/master/examples/fib/fib.sml
   Modified for use with MLKit
 *)

local
val grain  = CommandLineArgs.parseInt "G" 30    (* -G n *)
val N      = CommandLineArgs.parseInt "N" 39    (* -N n *)

fun sfib n =
  if n <= 1 then n else sfib (n-1) + sfib (n-2)

fun fib n =
  if n <= grain then sfib n
  else let val (x,y) = ForkJoin.par (fn _ => fib (n-1), fn _ => fib (n-2))
       in x + y
       end

in
val () =
    Timing.run ("Calculating fib " ^ Int.toString N ^ " (grain = " ^ Int.toString grain ^ ")")
               (fn {endtiming} =>
                   let val f = fib N
                       val () = endtiming()
                   in  f = 63245986 (* N = 39 *)
                   end)
end
