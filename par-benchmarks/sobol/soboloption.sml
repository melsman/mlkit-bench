
structure Sobol = Sobol(val D = 1
                        structure SobolDir = SobolDir50)

structure S = SOAC

(* some utilities *)

fun sq (x:real) = x * x
fun max (a,b):real = if a > b then a else b

infix |>
fun v |> f = f v

(* finance utilities *)

fun invCumNormDist p =
    if p < 0.0 then raise Fail "invCumNormDist called with arg < 0.0"
    else if p > 1.0 then raise Fail "invCumNormDist called with arg > 1.0"
    else
    (* assume p between 0 and 1 *)
    let val q = p - 0.5
        val a = if q < 0.0 then ~q else q
    in
      (* Rational approximation for central region *)

      if a <= 0.47575 then
        let val r = q*q
            val z = ((((( ~39.69683028665376 * r + 220.9460984245205) * r
                        - 275.9285104469687) * r + 138.3577518672690) * r
                        - 30.66479806614716) * r + 2.506628277459239) * q
                 / ((((( ~54.47609879822406  * r + 161.5858368580409) * r
                        - 155.6989798598866) * r + 66.80131188771972) * r
                        - 13.28068155288572) * r + 1.0)
        in z
        end

      (* Rational approximation for tails *)

      else

        (* If in upper tail, map to lower tail *)

        let val p = if q > 0.0 then 1.0 - p else p
            val r  = Math.sqrt(~2.0 * Math.ln p)
            val z = (((((~0.007784894002430293 * r - 0.3223964580411365)  * r
                         -2.400758277161838)   * r - 2.549732539343734)   * r
                         +4.374664141464968)   * r + 2.938163982698783)
                  / ((((  0.007784695709041462 * r + 0.3224671290700398)  * r
                        + 2.445134137142996)   * r + 3.754408661907416)   * r
                        + 1.0)

            (* If in upper tail, swap sign *)

            val z = if q > 0.0 then ~z else z
        in z
        end
    end

(* granularity control specification *)

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

  local (* option definition *)

    (*
       ---------------------------------------------------------------------------------------------------------
       Ex 1a: Equity Call Option (no settlement period, no dividend)
       Flirst code:
        flow(2009-04-01,"EUR",max(float(0.000000),sub(model("BASF",2009-04-01),float(45.000000))),"0")
       S0 = 48
       Risk free constant rate: 5%
       Continuous discounting
       Volatility (proportionality constant): 20%
       Valuation date: 2008-09-01
       Convention: Act/365
       Expected Value: 5.437419207 by Black-Scholes closed form, 5.437414 by SCD Black-Scholes with 10^7 paths
       FX's = []
       ----------------------------------------------------------------------------------------------------------
    *)

    val S0 = 48.0     (* C2 *)
    val K = 45.0      (* D2 *)
    val r = 0.05      (* E2 *)
    val sigma = 0.2   (* F2 *)
    val T = 7.0/12.0  (* G2 *)
  in
    fun St n =
        S0 * Math.exp((r - sq sigma / 2.0) * T + sigma * n * Math.sqrt T)
    fun callOptionPayOff P =
        Math.exp(~ r * T) * max(P - K, 0.0)
  end

  val endTiming = Timing.start "Computing optionpi..."
  val vs = S.map (fn i =>
                     let val v = Sobol.independent i
                         val x = conv(Array.sub(v,0))
                     in invCumNormDist x |>
                        St |>
                        callOptionPayOff
                     end) (S.iota N)
  val r = S.reduce gcs (op +) 0.0 vs
  val p = r / real N
  val () = endTiming()
in
  fun ppr r = Real.fmt (StringCvt.FIX (SOME 12)) r
  val () = print ("Option price: " ^ ppr p ^ " (" ^ Int.toString N ^ " paths)\n")
(*
  val expected = 5.437419207
  val () = print ("Precision (against " ^ ppr expected ^ "): "
                  ^ ppr (Real.abs(expected - p)) ^ "\n")  (* compared to closed form value *)
*)
  (* failure to converge towards the expected value
   * may be due to the invCumNormDist function *)
(*
  val () = print ("SCD Precision (10^7 paths): " ^ ppr (Real.abs(5.437414 - expected)) ^ "\n")
*)
end
