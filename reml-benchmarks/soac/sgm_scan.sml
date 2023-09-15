
structure S = SOAC

val gcs : S.gcs =
    (CommandLineArgs.parseInt "P" 50,
     CommandLineArgs.parseInt "G" 50000)

val N = CommandLineArgs.parseInt "N" 10000000

fun sgmscanarr () =
    let val a0 = S.map (fn i => (i mod 999 = 0, Int64.fromInt i)) (S.iota N)
    in S.sgm_scan__inline gcs Int64.+ 0 a0
    end

val () = Timing.run ("Segmented scanning " ^ Int.toString N ^ " numbers")
                    (fn {endtiming} =>
                        let val b = sgmscanarr()
                            val () = endtiming()
                            val m = S.reduce__inline gcs Int64.max ~100000 b
                        in m = 9989490510 andalso N = 10000000
                        end)
