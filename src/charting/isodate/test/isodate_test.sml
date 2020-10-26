(* Copyright 2015, Martin Elsman, MIT-license *)

fun pr s = print ("ISODate." ^ s ^ "\n")

fun itest s expected n =
    if n=expected then pr (s ^ ": OK")
    else pr(s ^ ": ERR - expected " ^ Int.toString expected ^ " but got " ^ Int.toString n)

fun stest s expected n =
    if n=expected then pr (s ^ ": OK")
    else pr(s ^ ": ERR - expected " ^ expected ^ " but got " ^ n)

fun popt NONE = "NONE"
  | popt (SOME s) = "SOME(" ^ s ^ ")"
fun ostest (s:string) (expected:string option) (n:string option) =
    if n=expected then pr (s ^ ": OK")
    else pr(s ^ ": ERR - expected " ^ popt expected ^ " but got " ^ popt n)

fun rtest s expected n =
    if Real.toString n = Real.toString expected then pr (s ^ ": OK")
    else pr(s ^ ": ERR - expected " ^ Real.toString expected ^ " but got " ^ Real.toString n)

structure D = ISODate
structure MD = D.MonthDay

fun fromString s = case D.fromString s of
                       SOME d => d
                     | NONE => (pr "err"; raise Fail "err")

val () = stest "t1" "2014-01-01" (D.toString (fromString "2014-01-01"))
val () = stest "t2" "2014-11-11" (D.toString (D.prev(fromString "2014-11-12")))
val () = stest "t3" "2014-10-31" (D.toString (D.prev(fromString "2014-11-01")))
val () = stest "t4" "2013-12-31" (D.toString (D.prev(fromString "2014-01-01")))

val () = stest "t5" "2013-11-05" (D.toString (D.next(fromString "2013-11-04")))
val () = stest "t6" "2013-11-01" (D.toString (D.next(fromString "2013-10-31")))
val () = stest "t7" "2014-01-01" (D.toString (D.next(fromString "2013-12-31")))

val () = stest "t8" "2017-03-22" (D.toString (D.nextBusiness(fromString "2017-03-21")))
val () = stest "t9" "2017-03-24" (D.toString (D.nextBusiness(fromString "2017-03-23")))
val () = stest "t10" "2017-03-27" (D.toString (D.nextBusiness(fromString "2017-03-24")))
val () = stest "t11" "2017-03-27" (D.toString (D.nextBusiness(fromString "2017-03-25")))

val () = itest "t12" 0 (D.minus (fromString "2017-03-25", fromString "2017-03-25"))
val () = itest "t13" 1 (D.minus (fromString "2017-03-26", fromString "2017-03-25"))

val () = stest "t14" "1456876800000" (Real.fmt (StringCvt.FIX(SOME 0)) (D.epoch (fromString "2016-03-02")))
val () = stest "t15" "1456963200000" (Real.fmt (StringCvt.FIX(SOME 0)) (D.epoch (fromString "2016-03-03")))   (* 36*24+14568768 = 14569632 *)

val mds = List.map (Option.valOf o MD.fromString) ["02/23","05/12", "12/15"]
val mkDate = D.toDate o Option.valOf o D.fromString

val () = itest "itest1" 3 (MD.numNext mds (fromString "2015-02-20"))
val () = itest "itest2" 0 (MD.numNext mds (fromString "2015-02-23"))
val () = itest "itest3" 77 (MD.numNext mds (fromString "2015-02-24"))
val () = itest "itest4" 68 (MD.numNext mds (fromString "2015-12-17"))  (* 14+31+23=68 *)

val mds2 = List.map (Option.valOf o MD.fromString) ["02/29","12/31"]

val () = itest "itest5" 0 (MD.numNext mds2 (fromString "2015-02-28"))  (* 0 *)

fun rep NONE = NONE
  | rep (SOME d) = SOME (D.toString d)

val () = ostest "test1" NONE (rep(D.fromString "2016-02-30"))
val () = ostest "test2" (SOME "2004-02-29") (rep(D.fromString "2004-02-29"))
val () = ostest "test3" NONE (rep(D.fromString "2003-02-29"))
val () = ostest "test4" NONE (rep(D.fromString "2003-00-29"))
val () = ostest "test5" NONE (rep(D.fromString "2003-01-00"))

val () = stest "test100" "1900-01-01" (D.toString (D.fromExcel1900 1))
val () = stest "test101" "2008-01-01" (D.toString (D.fromExcel1900 39448))
val () = stest "test102" "2011-05-22" (D.toString (D.fromExcel1900 40685))
val () = stest "test103" "2011-02-23" (D.toString (D.fromExcel1900 40597))
val () = stest "test104" "1998-07-05" (D.toString (D.fromExcel1900 35981))
val () = stest "test105" "1900-02-28" (D.toString (D.fromExcel1900 59))
val () = stest "test106" "1900-03-01" (D.toString (D.fromExcel1900 60))   (* Two numbers result in the same date *)
val () = stest "test107" "1900-03-01" (D.toString (D.fromExcel1900 61))   (* Excel maps both day 60 and day 61 to 1900-03-01 *)

val () = stest "test200" "1904-01-02" (D.toString (D.fromExcel1904 1))
val () = stest "test201" "2008-01-01" (D.toString (D.fromExcel1904 (39448-1462)))
val () = stest "test202" "2011-05-22" (D.toString (D.fromExcel1904 (40685-1462)))
val () = stest "test203" "2011-02-23" (D.toString (D.fromExcel1904 (40597-1462)))
val () = stest "test204" "1998-07-05" (D.toString (D.fromExcel1904 34519))

val () = stest "test300" "2015-12-15" (D.toString (MD.prevDate mds (fromString "2016-02-05")))

(* zipTimeseries test *)
(* unordered, missing data (in both lists) in start, end and middle *)

fun zipTest testName toString expected actual =
    let fun aux xs = "[" ^
                     String.concatWith
                         ", "
                         (map (fn (d, (v1, v2)) =>
                                  "(" ^ ISODate.toString d ^ ", (" ^
                                  toString v1 ^ ", " ^ toString v2 ^
                                  "))") xs) ^
                     "]"
        val expected = aux expected
        val actual = aux actual
    in if expected = actual then
           pr (testName ^ ": OK")
       else
           pr (testName ^ ": Fail. Expected " ^ expected ^ ", got " ^ actual)
    end


val () = zipTest "test400" Real.toString [(fromString "2018-01-01", (1.0, 2.0))]
                 (D.zipTimeseries ([(fromString "2018-01-01", 1.0)], [(fromString "2018-01-01", 2.0)]))


val () = zipTest "test401" Real.toString [(fromString "2018-01-01", (1.0, 1.1)),
                                          (fromString "2018-02-01", (2.0, 2.1))]
                 (D.zipTimeseries ([(fromString "2018-01-01", 1.0), (fromString "2018-02-01", 2.0)],
                                   [(fromString "2018-01-01", 1.1), (fromString "2018-02-01", 2.1)]))

val () = zipTest "test402" Real.toString [(fromString "2018-01-01", (1.0, 1.1)),
                                          (fromString "2018-02-01", (2.0, 2.1))]
                 (D.zipTimeseries ([(fromString "2018-01-01", 1.0), (fromString "2018-02-01", 2.0)],
                                   [(fromString "2018-02-01", 2.1), (fromString "2018-01-01", 1.1)]))

val () = zipTest "test403" Real.toString [(fromString "2018-01-01", (1.1, 1.0)),
                                          (fromString "2018-02-01", (2.1, 2.0))]
                 (D.zipTimeseries ([(fromString "2018-02-01", 2.1), (fromString "2018-01-01", 1.1)],
                                   [(fromString "2018-01-01", 1.0), (fromString "2018-02-01", 2.0)]))

val () = zipTest "test403" Real.toString [(fromString "2018-01-01", (1.1, 1.0)),
                                          (fromString "2018-02-01", (2.1, 2.0))]
                 (D.zipTimeseries ([(fromString "2018-02-01", 2.1), (fromString "2018-01-01", 1.1),
                                    (fromString "2018-03-01", 1.1)],
                                   [(fromString "2018-01-01", 1.0), (fromString "2018-02-01", 2.0)]))

fun toString x = getOpt (Option.map Real.toString x, "N/A")


val () = zipTest "test500" toString [(fromString "2018-01-01", (SOME 1.0, SOME 2.0))]
                 (D.zipTimeseriesPartial ([(fromString "2018-01-01", 1.0)], [(fromString "2018-01-01", 2.0)]))


val () = zipTest "test501" toString [(fromString "2018-01-01", (SOME 1.0, SOME 1.1)),
                                          (fromString "2018-02-01", (SOME 2.0, SOME 2.1))]
                 (D.zipTimeseriesPartial ([(fromString "2018-01-01", 1.0), (fromString "2018-02-01", 2.0)],
                                   [(fromString "2018-01-01", 1.1), (fromString "2018-02-01", 2.1)]))

val () = zipTest "test502" toString [(fromString "2018-01-01", (SOME 1.0, SOME 1.1)),
                                          (fromString "2018-02-01", (SOME 2.0, SOME 2.1))]
                 (D.zipTimeseriesPartial ([(fromString "2018-01-01", 1.0), (fromString "2018-02-01", 2.0)],
                                   [(fromString "2018-02-01", 2.1), (fromString "2018-01-01", 1.1)]))

val () = zipTest "test503" toString [(fromString "2018-01-01", (SOME 1.1, SOME 1.0)),
                                          (fromString "2018-02-01", (SOME 2.1, SOME 2.0))]
                 (D.zipTimeseriesPartial ([(fromString "2018-02-01", 2.1), (fromString "2018-01-01", 1.1)],
                                   [(fromString "2018-01-01", 1.0), (fromString "2018-02-01", 2.0)]))

val () = zipTest "test504" toString
                 [(fromString "2018-01-01", (SOME 1.1, SOME 1.0)),
                  (fromString "2018-02-01", (SOME 2.1, SOME 2.0)),
                  (fromString "2018-03-01", (SOME 1.3, NONE))]
                 (D.zipTimeseriesPartial
                      ([(fromString "2018-02-01", 2.1), (fromString "2018-01-01", 1.1),
                        (fromString "2018-03-01", 1.3)],
                       [(fromString "2018-01-01", 1.0), (fromString "2018-02-01", 2.0)]))

val () = zipTest "test505" toString
                 [(fromString "2018-01-01", (SOME 1.1, SOME 1.0)),
                  (fromString "2018-02-01", (NONE, SOME 2.0)),
                  (fromString "2018-03-01", (SOME 1.3, NONE))]
                 (D.zipTimeseriesPartial
                      ([(fromString "2018-01-01", 1.1), (fromString "2018-03-01", 1.3)],
                       [(fromString "2018-01-01", 1.0), (fromString "2018-02-01", 2.0)]))

val () = zipTest "test506" toString
                 [(fromString "2018-01-01", (SOME 1.1, NONE)),
                  (fromString "2018-03-01", (SOME 1.3, NONE))]
                 (D.zipTimeseriesPartial
                      ([(fromString "2018-01-01", 1.1), (fromString "2018-03-01", 1.3)],
                       []))

val () = zipTest "test507" toString
                 [(fromString "2018-01-01", (NONE, SOME 1.1)),
                  (fromString "2018-03-01", (NONE, SOME 1.3))]
                 (D.zipTimeseriesPartial
                      ([],
                       [(fromString "2018-01-01", 1.1), (fromString "2018-03-01", 1.3)]))

val () = zipTest "test508" toString
                 [(fromString "2018-01-01", (SOME 1.0, SOME 1.1)),
                  (fromString "2018-03-01", (NONE, SOME 1.3))]
                 (D.zipTimeseriesPartial
                      ([(fromString "2018-01-01", 1.0)],
                       [(fromString "2018-01-01", 1.1), (fromString "2018-03-01", 1.3)]))


(* zipTimeseriesPartial test *)
