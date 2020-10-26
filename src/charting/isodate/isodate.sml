(* Copyright 2015, Martin Elsman, MIT-license *)

structure ISODate :> ISO_DATE = struct

(* Internally, we represent a date as a word value. The least 5 bits
 * are used to represent the day (1-31). The next 4 bits are used for
 * representing the month (1-12) and the following bits are used to
 * represent the year.
 *
 * With this representation, which is quite compact, comparison of
 * dates reduce to comparison on the underlying
 * representation. Moreover, extracting the year, the month, or the
 * day amounts to shifting and masking.
 *)

fun die s = raise Fail ("ISODate." ^ s)

fun leapyear n = n mod 4 = 0 andalso (n mod 100 <> 0 orelse n mod 400 = 0)

fun daysMonth y m =
    case m of
        2 => if leapyear y then 29 else 28
      | 4 => 30
      | 6 => 30
      | 9 => 30
      | 11 => 30
      | _ => 31

type t = word
fun toYear (v:t) : word = Word.>>(v,0w9)
fun toMonth (v:t) : word = Word.andb(0wxF,Word.>>(v,0w5))
fun toDay (v:t) : word = Word.andb(0wx1F,v)
local
  fun fromYMDx {year,month,day} : t = Word.<<(Word.<<(year, 0w4) + month, 0w5) + day

  fun ppYMD {year,month,day} : string =
      "{year=" ^ Int.toString year ^
      ",month=" ^ Int.toString month ^
      ",day=" ^ Int.toString day ^
      "}"
in
fun fromYMDopt {year,month,day} =
    if year < 0 then NONE
    else if month <= 0 orelse month > 12 then NONE
    else if day <= 0 orelse day > daysMonth year month then NONE
    else SOME(fromYMDx{year=Word.fromInt year, month=Word.fromInt month, day=Word.fromInt day})

fun fromYMD d =
    case fromYMDopt d of
        SOME d => d
      | NONE => die ("fromYMD.invalid date: " ^ ppYMD d)
end
fun date x = fromYMD x

fun toYMDx t = {year=toYear t, month=toMonth t, day=toDay t}
fun toYMD t = {year=Word.toInt(toYear t),month=Word.toInt(toMonth t),day=Word.toInt(toDay t)}

val year = Word.toInt o toYear
val month = Word.toInt o toMonth
val day = Word.toInt o toDay

fun eq (d1,d2) = d1=d2

fun lt (d1,d2) = Word.<(d1,d2)
fun lte (d1,d2) = Word.<=(d1,d2)
fun min (d1,d2) = if lt(d1,d2) then d1 else d2
fun max (d1,d2) = if lt(d1,d2) then d2 else d1

fun compare (d1,d2) = Word.compare(d1,d2)

fun pad n s =
    let val sz = size s
    in if sz >= n then s
       else pad n ("0" ^ s)
    end

fun toStringWithSep sep d =
    let val {year,month,day} = toYMD d
    in pad 4 (Int.toString year) ^ sep ^
       pad 2 (Int.toString month) ^ sep ^
       pad 2 (Int.toString day)
    end
fun toString d = toStringWithSep "-" d

fun next d =
    let val {year,month,day} = toYMD d
    in if day < daysMonth year month then
         fromYMD {year=year,month=month,day=day+1}
       else if month < 12 then
         fromYMD {year=year,month=month+1,day=1}
       else fromYMD {year=year+1,month=1,day=1}
    end

fun prev d =
    let val {year,month,day} = toYMD d
    in if day > 1 then
         fromYMD {year=year,month=month,day=day-1}
       else if month > 1 then
         fromYMD {year=year,month=month-1,day=daysMonth year (month-1)}
       else fromYMD {year=year-1,month=12,day=31}
    end

(* Reingold: Number of the day within the year: *)

fun dayinyear year month day =
    let val monthno = month-1
    in day - 1 + 31 * monthno
       - (if monthno > 1 then
	    (27 + 4 * monthno) div 10 - (if leapyear year then 1 else 0)
	  else 0)
    end

(* Reingold: Find the number of days elapsed from the (imagined)
   Gregorian date Sunday, December 31, 1 BC to the given date. *)

fun todaynumber {year,month,day} =
    let val prioryears = year - 1
    in dayinyear year month day
       + 1
       + 365 * prioryears
       + prioryears div 4
       - prioryears div 100
       + prioryears div 400
    end

fun weekday d =
    let val daynumber = todaynumber (toYMD d)
    in daynumber mod 7
    end

fun isWeekend d =
    let val wd = weekday d
    in wd = 0 orelse wd = 6
    end

fun nextSkip skip d =
    let val d = next d
    in if skip d then nextSkip skip d
       else d
    end

fun nextBusiness d = nextSkip isWeekend d

infix minus
fun d1 minus d2 =
    let val days1 = todaynumber (toYMD d1)
        val days2 = todaynumber (toYMD d2)
    in days1 - days2
    end

local
  fun daysSince1970 d =
      todaynumber (toYMD d) - todaynumber {year=1970,month=1,day=1}
in
  fun epoch d = real (daysSince1970 d) * 3600.0 * 24.0 * 1000.0
end

local
fun readNum (s,x,n) =
    let fun read(x,0,acc) = SOME acc
          | read(x,n,acc) =
            let fun r d = read (x+1,n-1,acc*10+d)
            in case String.sub(s,x) of
                   #"0" => r 0
                 | #"1" => r 1
                 | #"2" => r 2
                 | #"3" => r 3
                 | #"4" => r 4
                 | #"5" => r 5
                 | #"6" => r 6
                 | #"7" => r 7
                 | #"8" => r 8
                 | #"9" => r 9
                 | _ => NONE
            end
    in read(x,n,0)
    end
in
fun fromString s =
    if size s <> 10 orelse String.sub(s,4) <> #"-" orelse String.sub(s,7) <> #"-" then NONE
    else case (readNum(s,0,4), readNum(s,5,2), readNum(s,8,2)) of
             (SOME y, SOME m, SOME d) => fromYMDopt {year=y,month=m,day=d}
           | _ => NONE
end

fun toMonth 1 = SOME Date.Jan
  | toMonth 2 = SOME Date.Feb
  | toMonth 3 = SOME Date.Mar
  | toMonth 4 = SOME Date.Apr
  | toMonth 5 = SOME Date.May
  | toMonth 6 = SOME Date.Jun
  | toMonth 7 = SOME Date.Jul
  | toMonth 8 = SOME Date.Aug
  | toMonth 9 = SOME Date.Sep
  | toMonth 10 = SOME Date.Oct
  | toMonth 11 = SOME Date.Nov
  | toMonth 12 = SOME Date.Dec
  | toMonth _ = NONE

fun fromMonth Date.Jan = 1
  | fromMonth Date.Feb = 2
  | fromMonth Date.Mar = 3
  | fromMonth Date.Apr = 4
  | fromMonth Date.May = 5
  | fromMonth Date.Jun = 6
  | fromMonth Date.Jul = 7
  | fromMonth Date.Aug = 8
  | fromMonth Date.Sep = 9
  | fromMonth Date.Oct = 10
  | fromMonth Date.Nov = 11
  | fromMonth Date.Dec = 12


fun datedate {year,month,day} =
    Date.date {year=year,month=month,day=day,hour=0,minute=0,second=0,offset=SOME(Time.zeroTime)}
fun datedate' {year,month,day} =
    case toMonth month of
        SOME m => datedate {year=year,month=m,day=day}
      | NONE => die "datedate'"

fun toDate (d:t) : Date.date =
    let val {year,month,day} = toYMD d
    in datedate' {year=year,month=month, day=day}
    end

fun fromDate (d:Date.date) : t =
    fromYMD {year=Date.year d,
             month=fromMonth(Date.month d),
             day=Date.day d}

fun firstInYear date =
    fromYMD { year = year date,
              month = fromMonth Date.Jan,
              day = 1 }

fun firstInMonth date =
    fromYMD { year = year date,
              month = month date,
              day = 1 }

fun minus1Year date =
    fromYMD { year = year date - 1,
              month = month date,
              day = day date }

fun minus1Week date =
    fromDate (datedate { year = year date,
                         month = case toMonth (month date) of
                                     SOME m => m
                                   | NONE => die "minus1Week couldn't get month" ,
                         day = day date - 7 })

fun minusNMonths date N =
    let val y = year date
        val N = N mod 12
        val y = y - N div 12
        val m = month date
        val (m,y) = if m - N > 0 then (m - N, y)
                    else (m - N + 12, y - 1)
        val d = day date
    in fromDate (datedate' { year = y, month = m, day = d })
    end

fun fromEpoch r =
  let val daysSinceEpoch = r / 1000.0 / 3600.0 / 24.0
      val n = round daysSinceEpoch
      val date = Date.date { year = 1970, month = Date.Jan, hour = 0, minute = 0,
                             second = 0, offset = SOME(Time.zeroTime), day = 1 + n }
  in fromDate date end

fun zipTimeseries (xs, ys) : (t * (real * real)) list =
  let fun compare' ((d1, _), (d2, _)) = compare (d1, d2)
      fun aux [] _ acc = acc
        | aux _ [] acc = acc
        | aux ((d1, v1) :: xs) ((d2, v2) :: ys) acc =
          case compare (d1, d2) of
              EQUAL => aux xs ys ((d1, (v1, v2)) :: acc)
            | LESS => aux xs ((d2, v2) :: ys) acc
            | GREATER => aux ((d1, v1) :: xs) ys acc
  in rev (aux (Listsort.sort compare' xs) (Listsort.sort compare' ys) []) end

fun zipTimeseriesPartial (xs, ys) : (t * (real option * real option)) list =
  let fun compare' ((d1, _), (d2, _)) = compare (d1, d2)
      fun aux [] [] acc = acc
        | aux [] ((d, v) :: xs) acc = aux [] xs ((d, (NONE, SOME v)) :: acc)
        | aux ((d, v) :: xs) [] acc = aux xs [] ((d, (SOME v, NONE)) :: acc)
        | aux ((d1, v1) :: xs) ((d2, v2) :: ys) acc =
          case compare (d1, d2) of
              EQUAL => aux xs ys ((d1, (SOME v1, SOME v2)) :: acc)
            | LESS => aux xs ((d2, v2) :: ys) ((d1, (SOME v1, NONE)) :: acc)
            | GREATER => aux ((d1, v1) :: xs) ys ((d2, (NONE, SOME v2)) :: acc)
  in rev (aux (Listsort.sort compare' xs) (Listsort.sort compare' ys) []) end

type monthday = {month:int,day:int}
structure MonthDay = struct

fun check mm dd =
    (dd > 0) andalso (
      (mm = 2 andalso dd <= 29) orelse
      (List.exists (fn m => m = mm) [1,3,5,7,8,10,12] andalso dd <= 31) orelse
      (List.exists (fn m => m = mm) [4,6,9,11] andalso dd <= 30)
    )

fun isSep c = c = #"/" orelse c = #"-"   (* we support both, but / is the one we print *)

fun toString {month,day} =
    let fun pad s = if size s = 1 then "0" ^ s else s
        val m = pad(Int.toString month)
        val d = pad(Int.toString day)
    in m ^ "/" ^ d
    end

fun fromString s =
    case String.tokens (fn #"/" => true | #"-" => true
                        | _ => false) s of
        [mm,dd] => (case (Int.fromString mm, Int.fromString dd) of
                        (SOME mm, SOME dd) =>
                        if check mm dd then SOME {month=mm,day=dd}
                        else NONE
                      | _ => NONE)
      | _ => NONE

fun diffSameYear d d' =
    if d' >= d then SOME (d' minus d)
    else NONE

fun fromYMDfixNonLeapDay {year,month,day} =
    let val day = if month = 2 andalso day = 29 andalso not (leapyear year) then 28
                  else day
    in fromYMD {year=year,month=month,day=day}
    end

fun numNext1 {month,day} d : int =
    let val {year,...} = toYMD d
        val d1 = fromYMDfixNonLeapDay {year=year,month=month,day=day}
    in case diffSameYear d d1 of
           SOME diff => diff
         | NONE => let val d2 = fromYMDfixNonLeapDay {year=year + 1,month=month,day=day}
                   in d2 minus d
                   end
    end

fun numNext nil d = raise Fail "ISOdate.MonthDay.numNext called with empty list argument"
  | numNext [x] d = numNext1 x d
  | numNext (x::xs) d = Int.min (numNext1 x d, numNext xs d)

fun numPrev1 {month,day} d =
    let val {year,...} = toYMD d
        val d1 = fromYMDfixNonLeapDay {year=year,month=month,day=day}
    in case diffSameYear d1 d of
           SOME diff => diff
         | NONE => let val d2 = fromYMDfixNonLeapDay {year=year - 1,month=month,day=day}
                   in d minus d2
                   end
    end

fun numPrev nil d = raise Fail "ISOdate.MonthDay.numPrev called with empty list argument"
  | numPrev [x] d = numPrev1 x d
  | numPrev (x::xs) d = Int.min (numPrev1 x d, numPrev xs d)


fun least lt [x] = SOME x
  | least lt nil = NONE
  | least lt (x::xs) = case least lt xs of
                           NONE => SOME x
                         | SOME x0 => if lt(x,x0) then SOME x else SOME x0

fun prevDate mds d =
    let val mdis = map (fn md => (md,numPrev1 md d)) mds
        val ({month,day},_) = case least (fn ((_,i1),(_,i2)) =>  i1 < i2) mdis of
                                  SOME mdi => mdi
                                | NONE => die "prevDate.empty list of mds"
        val {year,month=m0,day=d0} = toYMD d
        val year = if month > m0 orelse (month=m0 andalso day > d0) then year-1
                   else year
    in fromYMD {year=year,month=month,day=day}
    end

end

fun dateFilter fromDate toDate f =
  List.filter (fn x => let val date = f x
                       in lte(fromDate, date) andalso
                          lte(date, toDate)
                       end)

fun fromExcel1900 n =
    if n < 1 then die "fromExcel1900 expects a positive argument"
    else let val n = if n <= 60 then n else n-1
         in fromDate (datedate {year=1900,month=Date.Jan,day=n}) (* Excel maps both day 60 and day 61 to 1900-03-01 *)
         end

fun fromExcel1904 n =
    if n < 1 then die "fromExcel1904 expects a positive argument"
    else fromDate (datedate {year=1904,month=Date.Jan,day=n+1})

end
