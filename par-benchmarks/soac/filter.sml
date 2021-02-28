
structure S = SOAC

val gcs : S.gcs =
    (CommandLineArgs.parseInt "P" 50,
     CommandLineArgs.parseInt "G" 5000000)

val N = CommandLineArgs.parseInt "N" 100000000

val a0 = S.map Int64.fromInt (S.iota N)
val a = S.toArray gcs 0 a0

fun filterarr () =
    S.Array.filter'__inline gcs (fn x => x mod 99 = 0) a

val () = Timing.run ("Filtering array of " ^ Int.toString N ^ " numbers")
                    (fn {endtiming} =>
                        let val b = filterarr()
                            val () = endtiming()
                            val c = S.reduce__inline gcs Int64.max ~1000 (S.fromArray b)
                        in c = 99999999 andalso Array.length b = 1010102
                        end)
