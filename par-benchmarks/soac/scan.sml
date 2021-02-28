
structure S = SOAC

val gcs : S.gcs =
    (CommandLineArgs.parseInt "P" 50,
     CommandLineArgs.parseInt "G" 100000)

val N = CommandLineArgs.parseInt "N" 100000000

val () = print ("Scanning " ^ Int.toString N ^ " numbers\n")

val endTiming = Timing.start "Scanning array"

val a0 = S.map Int64.fromInt (S.iota N)
val a = S.scan__inline gcs Int64.+ 0 a0
val () = endTiming()

val m = S.reduce__inline gcs Int64.max ~100000 (S.fromArray a)

val N' = Int64.fromInt N
val k = N' * (N'-1) div 2
val () = print ("Max value in scan: " ^ Int64.toString m ^ "\n")
val () = print ("N * (N-1) / 2: " ^ Int64.toString k ^ "\n")
