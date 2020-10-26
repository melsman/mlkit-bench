(* Copyright 2015, Martin Elsman, MIT-license *)

structure JsUtil = struct

fun die s = raise Fail ("JsUtil: " ^ s)

fun triggerResize () =
  (Js.setTimeout
      0
      (fn () =>
          JsCore.exec0 { res = JsCore.unit,
                         stmt = "window.dispatchEvent(new Event('resize'))" }
                       ());
   ())

fun removeChildren elem =
    case Js.firstChild elem of
        SOME c => (Js.removeChild elem c; removeChildren elem)
      | NONE => ()

infix &
open Js.Element

fun tag_sty t s e = taga t [("style",s)] e

fun mkFlexBox3 e1 e2 e3 =
    tag_sty "table" "width:100%;height:100%;border-spacing:0;border:none;" (
      tag "tr" (
        tag_sty "td" "width:50%;height:30px;padding:10px;text-align:left;" e1 &
        tag_sty "td" "width:50%;height:30px;padding:10px;text-align:right;" e2
      ) &
      tag_sty "tr" "height:100%;" (
        taga "td" [("colspan","2"),("style","width:100%;height:100%;")] e3
      )
    )

fun mkFlexBox2 e1 e2 =
    tag_sty "table" "width:100%;height:100%;border-spacing:0;border:none;" (
      tag "tr" (
        tag_sty "td" "width:100%;height:30px;padding:10px;text-align:left;" e1
      ) &
      tag_sty "tr" "height:100%;" (
        taga "td" [("style","width:100%;height:100%;")] e2
      )
    )

fun stack2 e1 e2 =
    tag_sty "table" "width:100%;height:100%;border-spacing:0;border:none;" (
      tag "tr" (
        taga "td" [("style","width:100%;height:50%;")] e1
      ) &
      tag "tr" (
        taga "td" [("style","width:100%;height:50%;")] e2
      )
    )

fun mkFlexGrid w (es: Js.elem list) : Js.elem =
    let fun split n nil acc = [rev acc]
          | split 0 es acc = rev acc :: split w es nil
          | split n (e::es) acc = split (n-1) es (e::acc)
        val ess = split w es nil
        val width = Int.toString (100 div w)
        fun genRow es = List.foldr (fn (e,acc) => taga "td" [("style","text-align:center;width:" ^ width ^ "%;")] e & acc) ($"") es   (* background-color:#F2F2F2;padding:15px; *)
        val rows = List.foldr (fn (es,acc) => tag "tr" (genRow es) & acc) ($"") ess
    in tag_sty "table" "border-collapse:separate;border-spacing:10px;" rows
    end

fun getElem id : Js.elem =
    case Js.getElementById Js.document id of
        SOME e => e
      | NONE => die ("getElem(" ^ id ^ ")")

fun onload (f: unit -> unit) : unit =
    let open JsCore infix ==>
    in exec1{arg1=("a", unit ==> unit),
             stmt="return window.onload=a;",
             res=unit} f
    end

fun linkElem (s:string) (f:unit->unit) : Js.elem =
    let val e = taga "span" [("class","link")] ($s)
    in Js.installEventHandler e Js.onclick (fn () => (f();true))
     ; e
    end

fun divc c e = taga "div" [("class",c)] e
fun divc0 c = taga0 "div" [("class",c)]

local
  fun td e = taga "td" [] e
  fun tdw0 w = taga0 "td" [("style","width:" ^ w)]
  fun tdw w e = taga "td" [("style","width:" ^ w)] e
  fun tr e = taga "tr" [] e
  fun table e = taga "table" [] e
in
fun pairElems left right =
    table (tr (tdw "50%" left & tdw "50%" right))
fun pairElems0 () =
    let val e1 = tdw0 "50%"
        val e2 = tdw0 "50%"
    in (table (tr (e1 & e2)), e1, e2)
    end
fun pairElemsPercent p left right =
  table (tr (tdw (Int.toString p ^ "%") left &
             tdw (Int.toString (100 - p) ^ "%") right))
fun stackElems top bot =
    table (tr (td top) & tr (td bot))
fun tableElems (xs : Js.elem list list) : Js.elem =
    case xs of
        nil => die "tableElems.empty table"
      | x::xs =>
        let val sz = List.length x
        in if sz < 1 then die "tableElems.row with too few elements"
           else
             let
               val w = Int.toString (100 div sz) ^ "%"
               fun layRow' [x] = tdw w x
                 | layRow' (x::xs) = tdw w x & layRow' xs
                 | layRow' nil = $""
               fun layRow xs = tr (layRow' xs)
               fun tab nil (SOME acc) = acc
                 | tab nil NONE = die "tableElems.no accumulator"
                 | tab (x :: xs) acc =
                     if length x <> sz then die "tableElems.rows of different sizes"
                     else case acc of
                              SOME e => tab xs (SOME(e & layRow x))
                            | NONE => tab xs (SOME(layRow x))
             in table (tab (x::xs) NONE)
             end
        end
end

fun alert s = JsCore.exec1 {arg1=("s",JsCore.string),res=JsCore.unit,stmt="alert(s);"} s
fun reloadSite () = JsCore.exec0{stmt="location.reload(true);",res=JsCore.unit} ()

fun mkFun0 f res =
  JsCore.exec1{stmt = "return function () { return f(); }",
               arg1 = ("f",JsCore.==>(JsCore.unit, res)),
               res = JsCore.fptr} f

fun mkFun1 f arg1 res =
  JsCore.exec1{stmt = "return function (x) { return f(x); }",
               arg1 = ("f",JsCore.==>(arg1, res)),
               res = JsCore.fptr} f

fun mkFun2 arg1 arg2 res f =
  JsCore.exec1{stmt = "return function (x, y) { return f([x, y]); }",
               arg1 = ("f",JsCore.===>(arg1, arg2, res)),
               res = JsCore.fptr} f

fun mkFun3 arg1 arg2 arg3 res f =
  JsCore.exec1{stmt = "return function (x, y, z) { return f([x, y, z]); }",
               arg1 = ("f",JsCore.====>(arg1, arg2, arg3, res)),
               res = JsCore.fptr} f

fun mkEventHandler (f : foreignptr -> unit) : foreignptr =
  JsCore.exec1 { arg1 = ("f", JsCore.==>(JsCore.fptr, JsCore.unit)),
                 res = JsCore.fptr,
                 stmt = "return function() { return f(this); };" }
               f

val null : foreignptr =
    JsCore.exec0 {res=JsCore.fptr,stmt="return null;"} ()

fun checkBox checked t (f : bool -> unit) : Js.elem  =
  let val e = taga0 "input" (("type","checkbox") ::
                             (if checked then [("checked", "true")] else []))
      val () = JsCore.exec2 { stmt = "elem.onchange = function(evt) { f(evt.target.checked) };",
                              arg1 = ("f", JsCore.==>(JsCore.bool, JsCore.unit)),
                              arg2 = ("elem", JsCore.fptr),
                              res = JsCore.unit }
                            (f, toForeignPtr e)
    in tag "label" (e & $t)
    end

fun replaceFirst parent new =
    case Js.firstChild parent of
        SOME c => Js.replaceChild parent new c
      | NONE => Js.appendChild parent new

end
