
structure S = SOAC

val gcs : S.gcs =
    (CommandLineArgs.parseInt "P" 10,
     CommandLineArgs.parseInt "G" 100000)

val N = CommandLineArgs.parseInt "N" 5000000

val () = print ("Segmented scanning " ^ Int.toString N ^ " numbers\n")

val endTiming = Timing.start "Scanning array"

val a0 = S.map (fn i => (i mod 999 = 0, Int64.fromInt i)) (S.iota N)
val a = S.sgm_scan__inline gcs Int64.+ 0 a0
val () = endTiming()

val m = S.reduce__inline gcs Int64.max ~100000 a

val () = print ("Max value in segmented scan: " ^ Int64.toString m ^ "\n")
