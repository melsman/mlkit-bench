
structure S = SOAC

val gcs : S.gcs =
    (CommandLineArgs.parseInt "P" 50,
     CommandLineArgs.parseInt "G" 20000)

val N = CommandLineArgs.parseInt "N" 1000000

val () = print ("Calculating the number of primes below or equal to " ^ Int.toString N ^ "\n")

local

fun concat_array (a1,a2) =
    let val sz1 = Array.length a1
    in Array.tabulate(sz1 + Array.length a2,
                      fn i => Array.sub(if i >= sz1
                                        then (a2,i-sz1)
                                        else (a1,i)))
    end

(* calculate primes below or equal to n *)
fun primes n : int array =
    if n < 2 then S.Array.empty()
    else let val c = Real.floor (Math.sqrt(real n))
             val ps0 = primes c
             val ps = S.fromArray ps0
             val is = S.toArray gcs 0 (S.map (fn x => x+c+1) (S.iota(n-c)))
             val fs = S.map (fn i =>
                                let val xs = S.map (fn p => if i mod p = 0 then 1 else 0) ps
                                in S.reduce__inline S.gcs_seq (op +) 0 xs
                                end) (S.fromArray is)
             val fs = S.toArray gcs 0 fs
             val new = S.Array.filter'__inline gcs (fn i => Array.sub(fs,i-c-1) = 0) is
         in concat_array (ps0, new)
         end

in
val () =
    Timing.run ("Calculating the number of primes below " ^ Int.toString N)
               (fn {endtiming} =>
                   let val ps = primes N
                       val () = endtiming()
                   in  Array.length ps = 78498
                   end)
end
