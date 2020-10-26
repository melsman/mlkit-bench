signature HIGHCHART = sig

  type chart

  val chartToFptr : chart -> foreignptr

  val setTitle : chart -> string -> unit

  val chartTitleLinker : string option -> (unit->unit) option -> JsCore.TypedObjects.obj JsCore.TypedObjects.j
                         -> JsCore.TypedObjects.obj JsCore.TypedObjects.j * (unit -> unit)

  val mkHighchart : JsCore.TypedObjects.obj JsCore.TypedObjects.j -> chart

  val pieChart : bool -> Js.elem -> string -> string -> string -> string -> string -> string -> (string * real option) list
                 -> string -> (string -> string -> (string -> string option) -> foreignptr -> string) -> unit

  val blockbarChart : Js.elem -> (string * real) list -> chart

  val timeSeriesChart : { chartRef : chart option ref,
                          chartdiv : Js.elem,
                          height : int option,
                          navigator : bool,
                          nullThreshold : bool,
                          ontitleClick : (unit -> unit) option,
                          plottype : string,
                          showLegend : bool,
                          title : string option,
                          ytitle : string option,
                          compare : bool,
                          onRedraw : (foreignptr -> unit) option,
                          rangeSelector : bool } ->
                        (string * (ISODate.t * real) list) list -> unit
(*
  val timeSeriesChartThumbnail : { chartdiv : Js.elem,
                                   title : string,
                                   plottype : string,
                                   nullThreshold : bool,
                                   showLegend : bool,
                                   height : int option } ->
                                 (string * (ISODate.t * real) list) list ->
                                 unit
*)

  val addTimeSeries : chart -> string * (ISODate.t * real) list -> unit

  val alphaChart : Js.elem -> foreignptr option ref
                   -> (string * (ISODate.t * real) list) list ->  unit

  val mkSeriesSimple : string * (ISODate.t * real) list -> foreignptr

  val mkSeries : string -> bool -> (string * (ISODate.t * real) list) list
                 -> JsCore.TypedObjects.obj JsCore.TypedObjects.arr JsCore.TypedObjects.j
  val reflow : chart -> unit

  val setyAxisTitle : chart -> string -> unit
  val removeSeries : chart -> unit

  structure BarChart :
            sig
              type t
              val barChart : Js.elem ->
                             {title       : string,
                              categories  : string list,
                              ytitle      : string,
                              ymeasure    : string,  (* e.g., "mm" or "sec" *)
                              series      : (string * real list) list
                             } -> t

              val setyAxisTitle : t -> string -> unit
              val setxAxisCategories : t -> string list -> unit
              val removeSeries : t -> unit
              val addSeries : t -> string * real list -> unit
            end

end

structure Highchart :> HIGHCHART =
struct

open Js.Element

open Prettyprint

fun die s = raise Fail ("Highchart: " ^ s)

structure JArr = JsCore.Array
structure JObj = JsCore.Object

type date = ISODate.t

(* Highchart utility functions *)
fun highchartsGetOptionsColor i : int =
    JsCore.exec1 {arg1=("i",JsCore.int),res=JsCore.int,
                  stmt="return Highcharts.getOptions().colors[i];"} i
fun highchartsSetOpacity (c:int) : int =
    JsCore.exec1 {arg1=("c",JsCore.int),res=JsCore.int,
                  stmt="return Highcharts.Color(c).setOpacity(0).get('rgba');"} c

fun formatter sign sz = JsCore.exec2{stmt="return function () { return this.y < sz ? null : this.point.name != 'NO'? this.point.name + ': ' + sign + this.y.toFixed(0) + '%' : null; };",
                                     arg1=("sign",JsCore.string), arg2=("sz",JsCore.int), res=JsCore.fptr} (sign,sz)

val posColors = List.rev ["#deebf7","#c6dbef","#9ecae1","#6baed6","#4292c6","#2171b5","#08519c"] (* from http://colorbrewer2.org/ *)
val negColors = List.rev ["#fee0d2","#fcbba1","#fc9272","#fb6a4a","#ef3b2c","#cb181d","#a50f15"]

fun mkData data colors =
    let val arr = JArr.empty()
        val c = ref 0
        fun getColor () = List.nth (colors,!c mod (length colors)) before c:= !c + 1
        val () = List.app (fn (s,r) =>
                              let val s = if s="" then "Unknown" else s
                                  val obj = JObj.fromList JsCore.string [("name",s)]
                                  val () = JObj.set JsCore.real obj "y" (Real.abs r)
                                  val c = if s = "NO" then "#ffffff"
                                          else getColor()
                                  val () = JObj.set JsCore.string obj "color" c
                              in JArr.push JsCore.fptr arr obj; ()
                              end) data
    in arr
    end

val sortGrp = Listsort.sort (fn ((s,x),(t,y)) => Real.compare (y,x))

fun lpct xs p =
    if Real.==(p,0.0) then raise Fail "div0"
    else sortGrp(List.map (fn (s,x) => (s,x* 100.0 / p)) xs)

fun splitData grp =
    let val pos = List.filter (fn (s,r) => r >= 0.0) grp
        val neg = List.filter (fn (s,r) => r < 0.0) grp
        val totpos = List.foldl (op +) 0.0 (List.map #2 pos)
        val totneg = List.foldl (op +) 0.0 (List.map #2 neg)
    in if totpos > abs totneg then
         (mkData (lpct pos totpos) posColors,mkData (lpct (("NO",~(totpos+totneg))::neg) totpos) negColors)
       else (* abs totneg >= totpos *)
         if Real.==(totneg,0.0) then (* here we know that also totpos = 0.0 *)
           (JArr.empty(),JArr.empty())
         else
           (mkData (lpct (("NO",~totneg-totpos)::pos) (abs totneg)) posColors,
            mkData (lpct neg (abs totneg)) negColors)
    end

type chart = foreignptr
fun chartToFptr (x: chart) : foreignptr = x

fun setTitle (c:chart) (t:string) : unit =
    let open JsCore.TypedObjects infix />
    in JsCore.method1 JsCore.fptr JsCore.unit c "setTitle"
                      (objToFptr(P "text" (S t)))
    end

fun mkHighchart obj: chart =
    JsCore.exec1 {stmt="return new Highcharts.Chart(obj);", res=JsCore.fptr,
                  arg1=("obj",JsCore.fptr)} (JsCore.TypedObjects.objToFptr obj)

local
    fun mkSerie title data size (formatter:foreignptr) innerSize =
      let open JsCore.TypedObjects infix />
          val obj =
              P "name" (S title) />
                P "data" (F data) />
                P "size" (S size) />
                P "dataLabels" (P "style" (P "fontSize" (I 10)) />
                                  P "formatter" (F formatter) />
                                  P "distance" (I ~32)) />
                P "showInLegend" (B false)
          val obj =
              case innerSize of
                  SOME sz => obj /> P "innerSize" (S sz)
                | NONE => obj
      in objToFptr obj
      end
in
fun pieChart0 thediv label kt valCur subtitle innerTitle outerTitle data legendprefix dataLabelMaxSzShow pointFormat =
    let fun look nil n = NONE
          | look ((x,r)::rest) n = if n = x then SOME(ppNumberRealStr r) else look rest n
        val title = label ^ " / " ^ kt
        val (outerData, innerData) = splitData data
        val labFormatter = labelFormatter (fn n => if n = "NO" then ""
                                                   else case look data n of
                                                            SOME v => n ^ ": " ^ legendprefix ^ " " ^ v
                                                          | NONE => "")
        val pntFormatter = pointFormatter (pointFormat label valCur (look data))
        val inner = mkSerie innerTitle innerData "60%" (formatter "-" dataLabelMaxSzShow) NONE          (* "-" is the sign *)
        val outer = mkSerie outerTitle outerData "100%" (formatter "" dataLabelMaxSzShow) (SOME "50%")
        val () = JObj.set JsCore.int outer "minSize" 100
        open JsCore.TypedObjects infix />
        val legend = P "align" (S "center") />
                     P "layout" (S "horizontal") />
                     P "verticalAlign" (S "bottom") />
                     P "x" (I 0) />
                     P "y" (I 0) />
                     P "enabled" (B true) />
                     P "itemMarginBottom" (I 5) />
                     P "itemStyle" (P "fontSize" (I 10)) />
                     P "labelFormatter" (F labFormatter)
        val chart = P "type" (S "pie") />
                    P "renderTo" (F (toForeignPtr thediv))
        val chart = case subtitle of
                        SOME _ => chart
                      | NONE => chart /> P "height" (I 130) /> P "margin" (A [I 0,I 0,I 0,I 0])
        val obj = P "chart" chart />
                    P "credits" (B false) />
                    P "tooltip" (P "pointFormatter" (F pntFormatter)) />
                    P "series" (A [F inner,F outer])
        val obj = case subtitle of
                      SOME st => obj /> P "subtitle" (P "text" (S st) /> P "align" (S "center") /> P "style" (P "fontSize" (I 10)))
                                     /> P "title" (P "text" (S title) /> P "align" (S "center") /> P "style" (P "fontSize" (I 12)))
                                     /> P "legend" legend
                                     /> P "plotOptions" (P "pie" (P "showInLegend" (B true)))

                    | NONE => obj /> P "exporting" (P "enabled" (B false))
                                  /> P "title" (P "text" (S label) /> P "align" (S "left") /> P "margin" (I 5) /> P "style" (P "fontSize" (I 10)))
                                  /> P "plotOptions" (P "pie" (P "size" (S "100%")))
    in (mkHighchart obj; ())
    end
end

fun pieChart thumb parentElem label kt valCur subtitle innerTitle outerTitle data legendprefix pointFormat =
    let val data = List.map (fn (x,SOME r) => (x,r) | _ => raise Fail "fail") data
        val subtitle = if thumb then NONE
                       else SOME subtitle
        val dataLabelMaxSzShow = if thumb then 20 else 5
    in pieChart0 parentElem label kt valCur subtitle innerTitle outerTitle data legendprefix dataLabelMaxSzShow pointFormat
    end handle Fail s => Js.appendChild parentElem ($("No chart: " ^ s))

(* Show a bar chart *)
fun blockbarChart parentElem (data0 : (string*real) list) : chart =
    let open JsCore.TypedObjects infix />
        val data = Listutil.withPct 1000 data0
        val series = List.map (fn (k,v,p) => P "name" (S k) /> P "data" (A [R p])) data
        val chart = P "type" (S "bar") />
                      P "renderTo" (F (toForeignPtr parentElem)) />
                      P "margin" (A [I 0,I 0,I 0,I 0]) />
                      P "spacing" (A [I 0,I 0,I 0,I 0])
        fun deconThis this =
            let val name = JsCore.getProperty (JsCore.getProperty this JsCore.fptr "series") JsCore.string "name"
                val pct = JsCore.getProperty this JsCore.real "y"
                val value = Listutil.findKey data0 name
                val pct = case value of
                              SOME v => if v < 0.0 then ~pct else pct
                            | NONE => pct
                val pct = ppNumberRealStr0 0 pct
            in (name,pct,value)
            end
        val labFormatter = pointFormatter (fn this => #2(deconThis this) ^ "%")
        val dataLabels = P "enabled" (B true) />
                           P "style" (P "color" (S "white") />
                                        P "textShadow" (S "0px 1px 2px black")) />
                           P "formatter" (F labFormatter)
        val tooltipFormatter =
            pointFormatter (fn this =>
                               let val (name,_,value) = deconThis this
                                   val value = case value of
                                                   SOME v => "$" ^ ppNumberRealStr v (*^ " (" ^ pct ^ "%)"*)
                                                 | NONE => "-"
                               in name ^ ": " ^ value
                               end)
        val obj = P "chart" chart />
                    P "credits" (P "enabled" (B false)) />
                    P "exporting" (P "enabled" (B false)) />
                    P "title" (P "text" (F JsUtil.null)) />
                    P "subtitle" (P "text" (F JsUtil.null)) />
                    P "xAxis" (P "visible" (B false)) />
                    P "yAxis" (P "visible" (B false) /> P "maxPadding" (I 0)) />
                    P "legend" (P "enabled" (B false)) />
                    P "tooltip" (P "formatter" (F tooltipFormatter)) />
                    P "plotOptions" (P "series" (P "stacking" (S "normal")) />
                                       P "bar" (P "groupPadding" (I 0) />
                                                  P "pointPadding" (I 0) />
                                                  P "dataLabels" dataLabels)) />
                    P "series" (A series)
    in mkHighchart obj
    end

fun arraySub (a:foreignptr, i:int) : foreignptr =
    JsCore.exec2{stmt="return a[i];", arg1=("a",JsCore.fptr), arg2=("i",JsCore.int), res=JsCore.fptr}(a,i)

fun setyAxisTitle (c:chart) (s:string) : unit =
    let val yAxis = JsCore.getProperty c JsCore.fptr "yAxis"
        val yAxis0 = arraySub(yAxis,0)
    in setTitle yAxis0 s
    end

fun removeSeries (c:chart) : unit =
    JsCore.exec1 {stmt="while (c.series.length > 0) { c.series[0].remove(); }",
                  arg1=("c",JsCore.fptr), res=JsCore.unit} c

structure BarChart = struct
type t = chart
fun barChart (parentElem:Js.elem) {title       : string,
                                   categories  : string list,
                                   ytitle      : string,
                                   ymeasure    : string,  (* e.g., "mm" or "sec" *)
                                   series      : (string * real list) list
                                  } : chart =
    let open JsCore.TypedObjects infix />
        val chart = P "type" (S "column") />
                    P "renderTo" (F (toForeignPtr parentElem))
        val fmt = if ymeasure = "lines" then ".0f"
                  else ".3f"
        val obj = P "chart" chart />
                  P "credits" (P "enabled" (B false)) />
                  P "exporting" (P "enabled" (B false)) />
                  P "title" (P "text" (S title)) />
                  P "subtitle" (P "text" (F JsUtil.null)) />
                  P "xAxis" (P "categories" (A (map S categories)) /> P "crosshair" (B true)) />
                  P "yAxis" (P "min" (I 0) /> P "title" (P "text" (S ytitle))) />
                  P "tooltip" (P "headerFormat" (S "<span style='font-size:10px'>{point.key}</span><table>") />
                               P "pointFormat" (S ("<tr><td style='color:{series.color};padding:0'>{series.name}: </td>" ^
                                                   "<td style='padding:0'><b>{point.y:" ^ fmt ^ "} " ^
                                                   ymeasure ^ "</b></td></tr>")) />
                               P "footerFormat" (S "</table>") />
                               P "shared" (B true) />
                               P "useHTML" (B true)) />
                  P "plotOptions" (P "column" (P "pointPadding" (R 0.2) />
                                                 P "boarderWidth" (I 0))) />
                  P "series" (A (map (fn (s,rs) => P "name" (S s) /> P "data" (A (map R rs))) series))
    in mkHighchart obj
    end

val setyAxisTitle = setyAxisTitle

fun setxAxisCategories (c:t) (categories:string list) : unit =
    let val xAxis = JsCore.getProperty c JsCore.fptr "xAxis"
        val xAxis0 = arraySub(xAxis,0)
        open JsCore.TypedObjects infix />
    in JsCore.method1 JsCore.fptr JsCore.unit xAxis0 "setCategories"
                      (arrToFptr(A (map S categories)))
    end

val removeSeries = removeSeries

fun addSeries (c:t) ((s,rs):string * real list) : unit =
    let open JsCore.TypedObjects infix />
        val obj = P "name" (S s) /> P "data" (A (map R rs))
    in JsCore.method1 JsCore.fptr JsCore.unit c "addSeries"
                      (objToFptr obj)
    end
end

fun mymap0 f nil ys = rev ys
  | mymap0 f (x::xs) ys = mymap0 f xs (f x::ys)
fun mymap f xs = mymap0 f xs nil

fun mkData (tseries:(ISODate.t*real)list) : real JsCore.TypedObjects.arr JsCore.TypedObjects.arr JsCore.TypedObjects.j =
  let open JsCore.TypedObjects infix />
      fun fromD d = ISODate.epoch d
      fun pair (d,v) = A[R(fromD d),R v]
  in A (mymap pair tseries) end

fun mkSeries plottype (nullThreshold:bool) (timeseries:(string*(ISODate.t*real)list)list) : JsCore.TypedObjects.obj JsCore.TypedObjects.arr JsCore.TypedObjects.j =
    let open JsCore.TypedObjects infix />
        fun mkSerie ((name,tseries),i) =
            let val color = highchartsGetOptionsColor i
                val colorOpaque = highchartsSetOpacity color
                val series =
                    P "name" (S name) />
                    P "type" (S plottype) />
                    P "tooltip" (P "valueDecimals" (I 2)) />
                    P "fillColor" (P "linearGradient" (P "x1" (I 0) />
                                                       P "y1" (I 0) />
                                                       P "x2" (I 0) />
                                                       P "y2" (I 1)) />
                                   P "stops" (A [A[I 0,I color],
                                                 A[I 1,I colorOpaque]])) />
                    P "data" (mkData tseries) />
                    P "showInLegend" (B true)
                val series = if nullThreshold then
                               series /> P "threshold" (F JsUtil.null)
                             else series
            in series
            end
    in A (Listutil.mapi mkSerie timeseries)
    end

fun mkSeriesSimple (name,tseries:(ISODate.t*real)list) : foreignptr =
    let open JsCore.TypedObjects infix />
        val series =
            P "name" (S name) />
              P "type" (S "line") />
              P "tooltip" (P "valueDecimals" (I 2)) />
              P "data" (mkData tseries)
    in objToFptr series
    end

fun addTimeSeries (c:chart) (name, tseries : (ISODate.t*real)list) : unit =
    let open JsCore.TypedObjects infix />
        val obj =
            P "name" (S name) />
              P "type" (S "line") />
              P "tooltip" (P "valueDecimals" (I 2)) />
              P "data" (mkData tseries)
    in JsCore.method1 JsCore.fptr JsCore.unit c "addSeries"
                      (objToFptr obj)
    end

fun highChart {chartdiv, chartRef: foreignptr option ref, obj: JsCore.TypedObjects.obj JsCore.TypedObjects.j} =
    let val () = JsUtil.removeChildren chartdiv
        val () = case !chartRef of
                     SOME ch => JsCore.exec1{res=JsCore.unit,arg1=("ch",JsCore.fptr),stmt="return ch.destroy();"} ch
                   | NONE => ()
        val obj = JsCore.TypedObjects.objToFptr obj
    in chartRef := SOME (JsCore.exec1 {stmt="return new Highcharts.StockChart(obj);", res=JsCore.fptr, arg1=("obj",JsCore.fptr)} obj)
     ; ()
    end

val newId =
    let val r = ref 0
    in fn () => (r:= !r+1; "smlid" ^ Int.toString (!r))
    end

fun chartTitleLinker (titleOpt:string option) (ontitleClick:(unit->unit)option) obj =
    let open JsCore.TypedObjects infix />
        val titleStyle = P "style" (P "fontSize" (I 12)) />
                         P "align" (S "left")
    in case (titleOpt, ontitleClick) of
           (NONE, _) => (obj /> P "title" (P "text" (F JsUtil.null)), fn() => ())
         | (SOME t, NONE) => (obj /> (P "title" (P "text" (S t)) />
                                      titleStyle),
                              fn () => ())
         | (SOME t, SOME f) =>
           let val id = newId()
               val link = "<span id='" ^ id ^ "' class='link'>" ^ t ^ "</span>"
           in (obj /> (P "title" (P "text" (S link) />
                                  titleStyle />
                                  P "useHTML" (B true))),
               fn () => case Js.getElementById Js.document id of
                            SOME e => Js.installEventHandler e Js.onclick (fn () => (f(); true))
                          | NONE => die "chart not installed")
           end
    end

(* plottype: "area" or "column" *)
fun timeSeriesChart {chartdiv, chartRef, title: string option, ytitle: string option, plottype: string,
                     nullThreshold:bool, showLegend:bool, navigator:bool,height:int option,
                     ontitleClick:(unit -> unit)option, compare : bool,
                     onRedraw : (foreignptr -> unit) option, rangeSelector : bool } ts =
    if List.null ts then ()
    else
    let open JsCore.TypedObjects infix />
        val chart = P "renderTo" (F(toForeignPtr chartdiv))
        val chart = case height of
                        SOME h => chart /> P "height" (I h)
                      | NONE => chart
        val chart = case ontitleClick of
                        SOME _ => chart /> P "spacing" (A[I 5, I 0, I 0, I 0])   (* it is a thumbnail *)
                      | NONE => chart
        val chart = case onRedraw of
                        SOME f => chart /> P "events" (P "redraw" (F (JsUtil.mkEventHandler f)))
                      | NONE => chart
        val chart = case ytitle of
                        SOME _ => chart /> P "spacing" (A[I 5, I 20, I 40, I 10])
                      | NONE => chart
        val series = mkSeries plottype nullThreshold ts
        val obj =
            P "chart" chart />
            P "credits" (B false) />
            P "rangeSelector" (if rangeSelector then P "selected" (I 5)
                               else P "enabled" (B false)) />
            P "series" series
        val obj = if navigator then obj
                  else obj /> (P "navigator" (P "enabled" (B false)) />
                               P "scrollbar" (P "enabled" (B false)))
        val obj = if showLegend then
                    obj /> P "legend" (P "enabled" (B true) (*/>*)
(*                                       P "align" (S"right") />
                                       P "floating" (B false) />
                                       P "width" (I 300) />
                                       P "layout" (S"horizontal") />
                                       P "verticalAlign" (S"top") />
                                       P "x" (I ~30)
*)
                                      )
                  else obj
        val obj = if plottype = "column" then
                    obj /> P "tooltip" (P "valueSuffix" (S "%"))
                        /> P "yAxis" (P "labels" (P "format" (S "{value}%")))
                  else if compare then
                      obj /> P "yAxis" (P "labels" (P "format" (S "{value}%")))
                  else obj
        val (obj,maybeSetupLink) = chartTitleLinker title ontitleClick obj
        val obj = if plottype = "area" then
                      let val series = (P "dataGrouping" (P "groupPixelWidth" (I 5)))
                          val series = if compare then
                                           series /> P "compare" (S "percent") />
                                                  P "compareBase" (I 100)
                                       else series
                      in obj /> (P "plotOptions" (P "area" (P "stacking" (S "normal")) />
                                                    P "series" series)) end
                  else if compare then
                      obj /> P "plotOptions" (P "series" (P "compare" (S "percent") />
                                                            P "compareBase" (I 100)))
                  else obj
        val obj = case ytitle of
                      SOME text =>
                      obj /> P "yAxis" (P "min" (I 0) /> P "title" (P "text" (S text)))
                    | NONE => obj
    in highChart {chartdiv=chartdiv, chartRef=chartRef, obj=obj}
     ; maybeSetupLink()
    end
(*
fun timeSeriesChartThumbnail {chartdiv, nullThreshold, showLegend, plottype,
                              title, height} series =
    let fun createBigChart parentElem =
            timeSeriesChart {chartdiv=parentElem,chartRef=ref NONE,title=SOME title,plottype=plottype,
                             nullThreshold=nullThreshold,showLegend=showLegend,navigator=true,
                             height=NONE,ontitleClick=NONE, compare = false, onRedraw = NONE,
                             rangeSelector = true } series
    in timeSeriesChart {chartdiv=chartdiv,chartRef=ref NONE,title=SOME title,plottype=plottype,
                        nullThreshold=nullThreshold,showLegend=false,navigator=false,
                        height=height,
                        ontitleClick=SOME (JsUtil.openDialogWithTitle
                                               title createBigChart),
                        compare = false, onRedraw = NONE, rangeSelector = true } series
    end
*)
fun alphaChart chartDiv chartRef ts =
    if List.null ts then ()
    else
    let open JsCore.TypedObjects infix />
        val chart = P "renderTo" (F (toForeignPtr chartDiv))
        val series = mkSeries "area" false ts
        val obj =
            P "chart" chart />
            P "credits" (B false) />
            P "rangeSelector" (P "selected" (I 5)) />
            P "series" series />
            P "navigator" (P "enabled" (B false)) />
            P "scrollbar" (P "enabled" (B false)) />
            P "legend" (P "align" (S"right") />
                          P "enabled" (B true) />
                          P "floating" (B false) />
                          P "width" (I 300) />
                          P "layout" (S"horizontal") />
                          P "verticalAlign" (S"top") />
                          P "x" (I ~30)) />
            P "yAxis" (P "labels" (P "formatter" (F (percentFormatter 0)))) />
            P "plotOptions" (P "area" (P "stacking" (S "normal")) />
                               P "series" (P "dataGrouping" (P "groupPixelWidth" (I 5))))
    in highChart {chartdiv=chartDiv, chartRef=chartRef, obj=obj}
    end

fun reflow (c:chart) : unit =
    JsCore.method0 JsCore.unit c "reflow"

end
