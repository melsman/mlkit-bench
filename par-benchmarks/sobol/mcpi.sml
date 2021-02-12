
structure Sobol = Sobol(val D = 2
                        structure SobolDir = SobolDir50)

structure S = SOAC

val gcs : S.gcs =
    (CommandLineArgs.parseInt "P" 10,
     CommandLineArgs.parseInt "G" 10000)

val N = CommandLineArgs.parseInt "N" 1000000

local
  fun conv x =  (* real(Word64.toLargeInt(Word32.toLarge x)) / Sobol.norm *)
      let val x' = Word32.>>(x,0w2)
          val y = Word32.andb(x,0w3)
      in (real (Word32.toInt y) + 4.0*real(Word32.toInt x'))
         / Sobol.norm
      end

  val endTiming = Timing.start "Computing pi..."
  val vs = S.map (fn i =>
                     let val v = Sobol.independent i handle _ => raise Fail "indep"
                         val x = conv(Array.sub(v,0)) handle _ => raise Fail ("x " ^ Word64.toString (Word32.toLarge (Array.sub(v,0))))
                         val y = conv(Array.sub(v,1)) handle _ => raise Fail "y"
                     in if x*x+y*y <= 1.0 then 1
                        else 0
                     end) (S.iota N)
  val r = S.reduce gcs (op +) 0 vs
  val pi = 4.0 * real r / real N
  val () = endTiming()
in
  fun ppr r = Real.fmt (StringCvt.FIX (SOME 12)) r
  val () = print ("PI: " ^ ppr pi ^ "\n")
  val () = print ("Precision: " ^ ppr (Real.abs(Math.pi - pi)) ^ "\n")
end
