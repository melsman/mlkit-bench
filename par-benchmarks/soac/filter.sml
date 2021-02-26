
structure S = SOAC

val gcs : S.gcs =
    (CommandLineArgs.parseInt "P" 50,
     CommandLineArgs.parseInt "G" 100000)

val N = CommandLineArgs.parseInt "N" 10000000

val () = print ("Scanning " ^ Int.toString N ^ " numbers\n")

val a0 = S.map Int64.fromInt (S.iota N)
val a = S.toArray gcs 0 a0

val endTiming = Timing.start "Filtering array"
val b = S.Array.filter'__inline gcs (fn x => x mod 99 = 0) a
val () = endTiming()

val c = S.reduce__inline gcs Int64.max ~1000 (S.fromArray b)
val () = print ("Max value in filtered array (size = " ^ Int.toString (Array.length b) ^ "): " ^ Int64.toString c ^ "\n")
