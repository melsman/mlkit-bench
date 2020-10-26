structure Prettyprint = struct

  open Js.Element

  fun decorate true e = taga "span" [("class","positive-number")] e
    | decorate false e = taga "span" [("class","negative-number")] e

  fun decorateStr true s = "<span class='positive-number'>" ^ s ^ "</span>"
    | decorateStr false s = "<span class='negative-number'>" ^ s ^ "</span>"

  fun mkThousandSeps whole =
      if size whole > 3 then
        mkThousandSeps (String.substring(whole,0,size whole - 3)) ^ "," ^ String.extract(whole,size whole - 3,NONE)
      else whole

  fun real_to_string_with_sep n r =
      let fun ppPos r =
              let val s = Real.fmt (StringCvt.FIX(SOME n)) r
              in case String.tokens (fn c => c = #".") s of
                     [whole,dec] => mkThousandSeps whole ^ "." ^ dec
                   | [whole] => mkThousandSeps whole
                   | _ => s
              end
      in if r < 0.0 then "-" ^ ppPos(~r) else ppPos r
      end

  fun ppNumberRealStr0 n r = decorateStr (r >= 0.0) (real_to_string_with_sep n r)
  fun ppNumberRealStr r = ppNumberRealStr0 2 r
  fun ppNumberReal0 n r = decorate (r >= 0.0) ($(real_to_string_with_sep n r))
  fun ppNumberReal r = ppNumberReal0 2 r
  fun ppNumber0 n s =
      case Real.fromString s of
          SOME r => ppNumberReal0 n r
        | NONE => $s

  fun ppBillions0 n r = real_to_string_with_sep n (r / 1000000000.0) ^ "B"
  fun ppBillions r = ppBillions0 2 r

  fun ppMillions0 n r = real_to_string_with_sep n (r / 1000000.0) ^ "M"
  fun ppMillions r = ppMillions0 2 r

  fun ppThousands0 n r = real_to_string_with_sep 2 (r / 1000.0) ^ "k"
  fun ppThousands r = ppThousands0 2 r

  fun ppNumber0' n r = real_to_string_with_sep n r
  fun ppNumber' r = real_to_string_with_sep 2 r
  fun ppNumber s = ppNumber0 2 s
  fun ppNumberStr0' n s =
    case Real.fromString s of
        SOME r => ppNumber0' n r
      | NONE => s
  fun ppNumberStr' s =
    ppNumberStr0' 2 s

  fun ppSmart0 n r =
    if r > 999999999.0 then
        ppBillions0 n r
    else if r > 999999.0 then
      ppMillions0 n r
    else if r > 999.0 then
      ppThousands0 n r
    else ppNumber0' n r

  fun ppSmart r = ppSmart0 2 r
  fun ppSmartStr s =
      case Real.fromString s of
          SOME r => ppSmart0 2 r
        | NONE => s

  fun ppPercent0' n r = real_to_string_with_sep n (r * 100.0) ^ "%"
  fun ppPercent' r = ppPercent0' 2 r
  fun ppPercent r = decorate (r >= 0.0) ($(ppPercent' r))
  fun ppPercentStr' s =
    case Real.fromString s of
        SOME r => ppPercent' r
      | NONE => s
  fun ppPercentStr s =
    case Real.fromString s of
        SOME r => ppPercent r
      | NONE => $s

  fun ppNumberOrInt s =
      if CharVector.exists (fn c => c= #".") s then ppNumber s
      else case IntInf.fromString s of
               SOME i => if i >= 0 then decorate (i >= 0) ($(mkThousandSeps s))
                         else decorate (i >= 0) ($("-" ^ mkThousandSeps (IntInf.toString (~i))))
             | NONE => ppNumber s
  fun ppNumberStr0 n s =
      case Real.fromString s of
          SOME r => ppNumberRealStr0 n r
        | NONE => s

  fun ppNumberStr s = ppNumberStr0 2 s

  fun ppNumberOrIntStr s =
      if CharVector.exists (fn c => c= #".") s then ppNumberStr s
      else case IntInf.fromString s of
               SOME i => if i >= 0 then decorateStr (i >= 0) (mkThousandSeps s)
                         else decorateStr (i >= 0) ("-" ^ mkThousandSeps (IntInf.toString (~i)))
             | NONE => ppNumberStr s

  fun ppRatio r =
      real_to_string_with_sep 0 r ^ ":1"

fun labelFormatter (f:string->string) : foreignptr =
    JsCore.exec1 {arg1=("f",JsCore.==>(JsCore.string,JsCore.string)),res=JsCore.fptr,
                  stmt="return function() { return f(this.name); };"} f

fun pointFormatter (f:foreignptr->string) : foreignptr =
    JsCore.exec1 {arg1=("f",JsCore.==>(JsCore.fptr,JsCore.string)),res=JsCore.fptr,
                  stmt="return function() { return f(this); };"} f

fun percentFormatter decimals =
    pointFormatter
        (fn this =>
            let val value = JsCore.Object.get JsCore.real this "value"
            in ppPercent0' decimals value end)

fun ratioFormatter decimals =
    pointFormatter
        (fn this =>
            let val value = JsCore.Object.get JsCore.real this "value"
            in real_to_string_with_sep decimals value ^ ":1" end)

end
