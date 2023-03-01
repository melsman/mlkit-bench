
structure S = SOAC

val gcs : S.gcs =
    (CommandLineArgs.parseInt "P" 50,
     CommandLineArgs.parseInt "G" 100000)

val N = CommandLineArgs.parseInt "N" 100000000

fun scanarr () =
    let val a0 = S.map Int64.fromInt (S.iota N)
    in S.scan__inline gcs Int64.+ 0 a0
    end

val () = Timing.run ("Scanning " ^ Int.toString N ^ " numbers")
                    (fn {endtiming} =>
                        let val a = scanarr()
                            val () = endtiming()
                            val m = S.reduce__inline gcs Int64.max ~100000 (S.fromArray a)
                            val N' = Int64.fromInt N
                            val k = N' * (N'-1) div 2
                        in k = m
                        end)
