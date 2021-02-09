
(* some utilities *)
infix |>
fun v |> f = f v

structure S = SOAC

val P = CommandLineArgs.parseInt "P" 10
val G = CommandLineArgs.parseInt "G" 100000

val gcs : S.gcs = (P,G)

val N = CommandLineArgs.parseInt "N" 10000000

val () = print ("Scanning " ^ Int.toString N ^ " numbers\n")

val endTiming = Timing.start "Scanning array"

val a0 = S.map Int64.fromInt (S.iota N)
val a = S.zscan__inline gcs Int64.+ 0 a0 |> S.fromArray
val () = endTiming()

val m = S.zreduce__inline gcs Int64.max ~100000 a

val N' = Int64.fromInt N
val k = N' * (N'-1) div 2
val () = print ("Max value in scan: " ^ Int64.toString m ^ "\n")
val () = print ("N * (N-1) / 2: " ^ Int64.toString k ^ "\n")
