
(* Some generic utilities *)

fun die s = raise Fail ("main.sml: " ^ s)

open Js.Element infix &

fun connect nil = $""
  | connect [e] = e
  | connect (e :: es) = e & connect es

fun collect (sel:'a->string) (xs:'a list) : string list =
    Listsort.sort
        String.compare
        (foldl (fn (l, acc) =>
                   let val c = sel l
                   in if Listutil.mem c acc then acc
                      else c::acc
                   end) nil xs)

fun getMachines (lines: Data.line list) : string list =
    let val d = List.filter (fn l => #cname l = "MLKIT") lines
    in collect #mach d
    end

fun getMachineVersion (lines: Data.line list) : string option =
    case getMachines lines of
        x :: _ => SOME x
      | _ => NONE

fun filebaseOfUrl s =
    case rev(String.tokens (fn c => c = #"/") s) of
        file :: _ => OS.Path.base file  (* drop extension *)
      | _ => die ("fileOfUrl: failed to parse url " ^ s)

fun decomposeFilebase (filebase:string) =
    case String.tokens (fn c => c = #"_") filebase of
        ["report",v,mt,d] => SOME (v,mt,d)
      | _ => NONE

fun avg nil = 0.0
  | avg xs = foldl (op +) 0.0 xs / (real (length xs))

fun cversionToCommitDate (cversion:string) : ISODate.t option =
    case String.tokens (fn c => c = #"(") cversion of
        [_,s] => (case String.tokens (fn c => c = #")") s of
                      [s,_] => let val d = String.extract (s,0,SOME 10)
                               in ISODate.fromString d
                               end
                    | _ => NONE)
      | _ => NONE

(* ------------------- *)
(* Some useful widgets *)
(* ------------------- *)

(* A generic selection widget to be installed at the provided element. It returns a function
 * that can be used to find the current value of the selection. *)
fun select (e:Js.elem) (options:(string*string)list) (onChange: string -> bool) : unit -> string =
    let val options' = map (fn (v,s) => taga "option" [("value",v)] ($s)) options
        val sel = taga "select" [("class", "custom-select custom-select-sm")]
                       (connect options')
        val () = Js.installEventHandler sel Js.onchange (fn () => onChange(Js.value sel))
    in Js.appendChild e sel
     ; fn () => Js.value sel
    end

(* A generic Bootstrap navBar that supports lazy evaluation of the underlying pages. *)

local
type 'a thunk = (unit -> 'a) ref
fun thunk (f: unit -> 'a) = ref f
fun eval (t: 'a thunk) =
    let val v = !t()
    in t := (fn () => v)
     ; v
    end
in
fun navbar (parent:Js.elem) (items: {pg_title:string, pg_gen: unit -> Js.elem} list) : unit =
    let val target_elem = taga0 "div" [("class","page-container")]
        val current : Js.elem option ref = ref NONE
        fun clearCurrent () =
            case !current of
                NONE => ()
              | SOME li => (Js.setAttribute li "class" "nav-item";
                            current := NONE)
        fun setCurrent li =
            (Js.setAttribute li "class" "nav-item active";
             current := SOME li)

        val lis =
            map (fn {pg_title=s,pg_gen=f} =>
                    let val title_elem = taga "a" [("class","nav-link"),("href","#")] ($s)
                        val li = taga "li" [("class","nav-item")] title_elem
                        val t = thunk f
                        fun onClickHandler () =
                            (clearCurrent();
                             setCurrent li;
                             JsUtil.removeChildren target_elem;
                             Js.appendChild target_elem (eval t);
                             true)
                        val () = Js.installEventHandler title_elem Js.onclick onClickHandler
                    in (li, onClickHandler)
                    end) items
        val () = case lis of (x,f)::_ => (f(); ()) | _ => ()
        val ul = taga "ul" [("class","navbar-nav")] (connect (map (#1) lis))
    in Js.appendChild parent
                      (taga "nav" [("class","navbar navbar-expand-sm navbar-light bg-light")]
                         (taga "a" [("class","navbar-brand"),("href","#")]
                               (taga0 "img" [("src","https://elsman.com/mlkit/images/mlkitsquare.png"),
                                            ("width","30"),
                                            ("height","30"),
                                            ("class","d-inline-block align-top"),
                                            ("alt","")] & ($" Benchmarking")) &
                          taga "button" [("class","navbar-toggler"),
                                         ("type","button"),
                                         ("data-toggle","collapse"),
                                         ("data-target","#navbarNav"),
                                         ("aria-controls","navbarNav"),
                                         ("aria-expanded","false"),
                                         ("aria-label","Toggle navigation")]
                              (taga0 "span" [("class","navbar-toggler-icon")]) &
                          taga "div" [("class","collapse navbar-collapse"),
                                      ("id","navbarNav")]
                              ul) &
                    tag "p" target_elem)
    end
end

(* A generic Highchart barChart widgets that supports reloading of series and which can be
 * controlled from a selection box. It also supports redrawing upon change of the underlying
 * data. *)

type dataspec = {kind:string,title:string,getnum:Data.line -> real}

fun genBarChart (picker_elem, graph_parent_elem) (dataspecs:dataspec list) (ymeasure:string)
                (getData:unit -> Data.line list) (redraw: (unit->unit) -> unit) : unit =
    let fun chartEntitiesFromDataSpec (data:Data.line list) (kind:string) =
            let val (ytitle,getnum: Data.line -> real) =
                  case List.find (fn ds => #kind ds = kind) dataspecs of
                      SOME ds => (#title ds, #getnum ds)
                    | NONE => ("unknown", fn _ => 0.0)
                val comps = collect #cname data
                val categories = collect #pname data
            in {ytitle=ytitle,
                categories= categories,
                series= map (fn cn =>
                                let val data = List.filter (fn l => #cname l = cn) data
                                    val rs = map (fn p =>
                                                     case List.filter (fn l => #pname l = p) data of
                                                         [l] => getnum l
                                                       | _ => 0.0) categories
                                in (cn, rs)
                                end) comps}
            end
        val k0 = case dataspecs of ds::_ => #kind ds | _ => "unknown"
        val c = let val {ytitle,categories,series} = chartEntitiesFromDataSpec (getData()) k0
                in Highchart.BarChart.barChart graph_parent_elem
                                               {title="",
                                                categories=categories,
                                                ytitle=ytitle,
                                                ymeasure=ymeasure,
                                                series=series}
                end
        fun reloadChart k =
            let val data = getData()
                val {ytitle,categories,series} = chartEntitiesFromDataSpec data k
            in Highchart.BarChart.setyAxisTitle c ytitle
             ; Highchart.BarChart.setxAxisCategories c categories
             ; Highchart.BarChart.removeSeries c
             ; app (Highchart.BarChart.addSeries c) series
             ; true
            end
        val select_items = map (fn ds => (#kind ds,#title ds)) dataspecs
        val getValue = if length dataspecs = 1 then fn () => k0
                       else select picker_elem select_items reloadChart
        val () = redraw (fn () => (reloadChart (getValue()); ()))
    in ()
    end

(* A generic Highchart line chart widgets that supports reloading of series and which can be
 * controlled from a selection box. It also supports redrawing upon change of the underlying
 * data. *)

fun genLineChart cname
                 (picker_elem, graph_parent_elem) (dataspecs:dataspec list) (ymeasure:string)
                 (getData:unit -> Data.line list) (redraw: (unit->unit) -> unit) : unit =
    let fun chartEntitiesFromDataSpec (data:Data.line list) (kind:string) =
            let val (ytitle,getnum: Data.line -> real) =
                  case List.find (fn ds => #kind ds = kind) dataspecs of
                      SOME ds => (#title ds, #getnum ds)
                    | NONE => ("unknown", fn _ => 0.0)
                val data = List.filter (fn l => #cname l = cname) data   (* pick the relevant data for cname *)
                val cversions = collect #cversion data
                val dates = collect (fn d => ISODate.toString(ISODate.fromDate (#date d))) data
                val pnames = collect #pname data
(*
                val () = JsUtil.alert ("Data: " ^ Int.toString(length data) ^ "\n" ^
                                       "cversions: " ^ Int.toString(length cversions) ^ "\n" ^
                                       "pnames: " ^ Int.toString(length pnames))
*)
                val series =
                    map (fn pn =>
                            let val data = List.filter (fn l => #pname l = pn) data
(*
                                val drs = List.mapPartial
                                              (fn cv =>
                                                  case cversionToCommitDate cv of
                                                      SOME d => (case List.filter (fn l => #cversion l = cv) data of
                                                                     [l] => SOME (d, getnum l)
                                                                   | _ => NONE)
                                                    | NONE => NONE) cversions
*)
                                val drs = List.mapPartial
                                              (fn d =>
                                                  case List.filter (fn l => ISODate.toString(ISODate.fromDate (#date l)) = d) data of
                                                      [l] => (case ISODate.fromString d of
                                                                  SOME d => SOME (d, getnum l)
                                                                | NONE => NONE)
                                                    | _ => NONE) dates
                            in (pn, drs)
                            end) pnames
(*                val series = [List.hd series] *)
            in {ytitle=ytitle,series=series}
            end
        val k0 = case dataspecs of ds::_ => #kind ds | _ => "unknown"
        val c = let val {ytitle,series} = chartEntitiesFromDataSpec (getData()) k0
                    (* memo: we should make use of ytitle, ymeasure *)
                    val chartRef=ref NONE
(*
                    fun ppSeries ss =
                        let fun ppSerie (n,trs) =
                                n ^":" ^ String.concatWith "," (map (fn (t,r) => ISODate.toString t ^ " : "
                                                                                 ^ Real.toString r) trs)
                        in String.concatWith "\n" (map ppSerie ss)
                        end
                    val () = JsUtil.alert (ppSeries series)
*)
                in Highchart.timeSeriesChart {chartdiv=graph_parent_elem, chartRef=chartRef,
                                              ytitle=SOME ytitle,
                                              title=NONE, plottype="line",
                                              nullThreshold=true, showLegend=true, navigator=false,
                                              height=NONE, ontitleClick=NONE, compare=false, onRedraw=NONE,
                                              rangeSelector=false} series
                 ; (case !chartRef of
                        SOME c => c
                      | NONE => die "genLineChart.expecting chart reference")
                end
        fun reloadChart k =
            let val data = getData()
                val {ytitle,series} = chartEntitiesFromDataSpec data k
            in Highchart.setyAxisTitle c ytitle
             ; Highchart.removeSeries c
             ; app (Highchart.addTimeSeries c) series
             ; true
            end
        val select_items = map (fn ds => (#kind ds,#title ds)) dataspecs
        val getValue = if length dataspecs = 1 then fn () => k0
                       else select picker_elem select_items reloadChart
        val () = redraw (fn () => (reloadChart (getValue()); ()))
    in ()
    end


(* --------- *)
(* The pages *)
(* --------- *)

(* The snapshot page *)
structure SnapshotPage :
  sig
    val setDataset : (string * Data.line list) list -> unit
    val elem       : Js.elem
  end =
struct
  val report_picker_elem = tag0 "div"
  val date_elem = tag0 "div"
  val commitdate_elem = tag0 "div"
  val mach_elem = tag0 "div"

  val header_block_elem =
      taga "div" [("class","card w-100")]
           (taga "div" [("class","card-body")]
                 (taga "h6" [("class","card-subtitle")] ($"Report Snapshot") &
                  tag "p" report_picker_elem &
                  taga "h6" [("class","card-subtitle")] ($"Computation date") &
                  tag "p" date_elem &
                  taga "h6" [("class","card-subtitle")] ($"Commit date") &
                  tag "p" commitdate_elem &
                  taga "h6" [("class","card-subtitle")] ($"Machine") &
                  tag "p" mach_elem
                 ))

  val exectime_picker_elem = tag0 "div"
  val exectime_graph_elem = tag0 "div"

  val memusage_picker_elem = tag0 "div"
  val memusage_graph_elem = tag0 "div"

  val comptime_picker_elem = $""  (* dummy - not used *)
  val comptime_graph_elem = tag0 "div"

  val plen_picker_elem = $""  (* dummy - not used *)
  val plen_graph_elem = tag0 "div"

  val exec_block_elem =
      taga "div" [("class","card w-100")]
           (taga "div" [("class","card-body")]
                 (tag "h2" ($"Execution Time") &
                  tag "p" exectime_picker_elem &
                  tag "p" exectime_graph_elem &
                  tag "h2" ($"Memory Usage") &
                  tag "p" memusage_picker_elem &
                  tag "p" memusage_graph_elem &
                  tag "h2" ($"Compilation Time") &
                  tag "p" comptime_graph_elem &
                  tag "h2" ($"Program Length") &
                  tag "p" plen_graph_elem))

  val elem =
      tag "div"
          (tag "p" header_block_elem &
               tag "p" exec_block_elem)

  local
    val data : Data.line list ref = ref nil
    val datalisteners : (unit -> unit) list ref = ref nil
  in
    fun getData () : Data.line list = !data
    fun setData (d:Data.line list) =
        (data := d; List.app (fn f => f()) (!datalisteners))
    fun redraw f = datalisteners := f :: (!datalisteners)
  end

  fun setMachine_elem mach =
      (JsUtil.removeChildren mach_elem;
       Js.appendChild mach_elem ($mach))

  fun setDate_elem date =
      (JsUtil.removeChildren date_elem;
       Js.appendChild date_elem ($date))

  fun setCommitdate_elem (date:string) =
      (JsUtil.removeChildren commitdate_elem;
       Js.appendChild commitdate_elem ($date))

  fun installGraphs () =
      let
        val execTimeDataSpecs : dataspec list =
            [{kind="real", title="Real time (sec)", getnum=avg o (map #real) o #runs},
             {kind="user", title="User time (sec)", getnum=avg o (map #user) o #runs},
             {kind="sys",  title="Sys time (sec)",  getnum=avg o (map #sys) o #runs}]

        val memUsageDataSpecs : dataspec list =
            [{kind="rss", title="Resident set size (kb)", getnum=avg o (map (real o #rss)) o #runs},
             {kind="binsz", title="Size of executable (kb)", getnum=real o #binsz}]

        val compTimeDataSpecs : dataspec list =
            [{kind="ctime", title="Compilation time (sec)", getnum= #ctime}]

        val plenDataSpecs : dataspec list =
            [{kind="plen",  title="Program length (lines)",  getnum=real o #plen}]


      in genBarChart (exectime_picker_elem, exectime_graph_elem)
                     execTimeDataSpecs "sec" getData redraw
       ; genBarChart (memusage_picker_elem, memusage_graph_elem)
                     memUsageDataSpecs "kb" getData redraw
       ; genBarChart (comptime_picker_elem, comptime_graph_elem)
                     compTimeDataSpecs "sec" getData redraw
       ; genBarChart (plen_picker_elem, plen_graph_elem)
                     plenDataSpecs "lines" getData redraw
      end

  fun setDataset (dataset:(string * Data.line list) list) : unit =
      let val dataset = map (fn (url,data) => (filebaseOfUrl url, data)) dataset
          val reports = map #1 dataset
          fun f r =
              case decomposeFilebase r of
                  SOME (v,mt,d) =>
                  let val data =
                          case Listutil.lookupOpt dataset r of
                              SOME data => data
                            | NONE => die "setDataset.no data"
                      val m = case getMachineVersion data of
                                  SOME m => m
                                | NONE => "-"
                      val commit_date =
                          case List.filter (fn l => #cname l = "MLKIT") data of
                              l :: _ => (case cversionToCommitDate (#cversion l) of
                                             SOME d => ISODate.toString d
                                           | NONE => "-")
                            | _ => "-"
                  in setDate_elem d
                   ; setMachine_elem m
                   ; setCommitdate_elem commit_date
                   ; setData data
                   ; true
                  end
                | NONE => die ("setDataset.expected file name format 'report_version_machine_YYYY-MM-DD' - got " ^ r)
          val e = tag0 "div"
          val _ = select e (map (fn v => (v,v)) reports) f
          val () = case reports of
                       r :: _ => (f r; ())
                     | _ => ()
      in Js.appendChild report_picker_elem e
       ; installGraphs ()
      end

end

structure HistoricPage : sig val elem       : Js.elem
                             val setDataset : (string * Data.line list) list -> unit
                         end =
struct

  val mach_picker_elem = tag0 "div"

  val header_block_elem =
      taga "div" [("class","card w-100")]
           (taga "div" [("class","card-body")]
                 (taga "h6" [("class","card-subtitle")] ($"Machine") &
                  tag "p" mach_picker_elem
                 ))

  val exectime_picker_mlkit_elem = tag0 "div"
  val exectime_graph_mlkit_elem = tag0 "div"
  val exectime_picker_mlkitnogc_elem = tag0 "div"
  val exectime_graph_mlkitnogc_elem = tag0 "div"


  val exec_block_elem =
      taga "div" [("class","card w-100")]
           (taga "div" [("class","card-body")]
                 (tag "h2" ($"Execution Time (mlkit)") &
                  tag "p" exectime_picker_mlkit_elem &
                  tag "p" exectime_graph_mlkit_elem) &
            taga "div" [("class","card-body")]
                 (tag "h2" ($"Execution Time (mlkit -no_gc)") &
                  tag "p" exectime_picker_mlkitnogc_elem &
                  tag "p" exectime_graph_mlkitnogc_elem))

  val elem =
      tag "div"
          (tag "p" header_block_elem &
               tag "p" exec_block_elem)

  local
    val data : Data.line list ref = ref nil
    val datalisteners : (unit -> unit) list ref = ref nil
  in
    fun getData () : Data.line list = !data
    fun setData (d:Data.line list) =
        (data := d; List.app (fn f => f()) (!datalisteners))
    fun redraw f = datalisteners := f :: (!datalisteners)
  end

  fun installGraphs () =
      let
        val execTimeDataSpecs : dataspec list =
            [{kind="real", title="Real time (sec)", getnum=avg o (map #real) o #runs},
             {kind="user", title="User time (sec)", getnum=avg o (map #user) o #runs},
             {kind="sys",  title="Sys time (sec)",  getnum=avg o (map #sys) o #runs}]
(*
        val memUsageDataSpecs : dataspec list =
            [{kind="rss", title="Resident set size (kb)", getnum=avg o (map (real o #rss)) o #runs},
             {kind="binsz", title="Size of executable (kb)", getnum=real o #binsz}]

        val compTimeDataSpecs : dataspec list =
            [{kind="ctime", title="Compilation time (sec)", getnum= #ctime}]

        val plenDataSpecs : dataspec list =
            [{kind="plen",  title="Program length (lines)",  getnum=real o #plen}]
*)
      in genLineChart "MLKIT"
                      (exectime_picker_mlkit_elem, exectime_graph_mlkit_elem)
                      execTimeDataSpecs "sec" getData redraw
       ; genLineChart "MLKIT [-no_gc]"
                      (exectime_picker_mlkitnogc_elem, exectime_graph_mlkitnogc_elem)
                      execTimeDataSpecs "sec" getData redraw
(*       ; genGraph (memusage_picker_elem, memusage_graph_elem)
                  memUsageDataSpecs "kb" getData redraw
       ; genGraph (comptime_picker_elem, comptime_graph_elem)
                  compTimeDataSpecs "sec" getData redraw
       ; genGraph (plen_picker_elem, plen_graph_elem)
                  plenDataSpecs "lines" getData redraw
*)
      end

  fun DarwinVersion v =
      let val ns = String.tokens (fn c => c= #".") v
      in List.all (CharVector.all Char.isDigit) ns
      end

  fun elimDarwinVersionFromMachine m =
      case String.tokens (fn c => c = #" ") m of
          "Darwin" :: v :: rest =>
          if DarwinVersion v then String.concatWith " " ("Darwin"::rest)
          else m
        | _ => m

  fun setDataset (dataset:(string * Data.line list) list) : unit =
      let val dataset = List.mapPartial (fn (url,data) =>
                                            case getMachineVersion data of
                                                SOME m => SOME (filebaseOfUrl url, data, m)
                                              | NONE => NONE) dataset
          val M : (string*Data.line list)list StringMap.map =
              List.foldr (fn ((f,d,m),M) =>
                             let val m = elimDarwinVersionFromMachine m
                             in case StringMap.lookup M m of
                                    SOME vs => StringMap.add(m,(f,d)::vs,M)
                                  | NONE => StringMap.add(m,[(f,d)],M)
                             end)
                         StringMap.empty dataset
          val machines = StringMap.dom M
          fun f m =
              let val fds = case StringMap.lookup M m of
                                SOME fds => fds
                              | NONE => die "setDataset.no data"
                  val data = List.concat(map #2 fds)
              in setData data
               ; true
              end
          val e = tag0 "div"
          val _ = select e (map (fn v => (v,v)) machines) f
          val () = case machines of
                       m :: _ => (f m; ())
                     | _ => ()
      in Js.appendChild mach_picker_elem e
       ; installGraphs ()
      end
end

(* The raw data page *)
structure RawDataPage : sig val addLinks : string list -> unit
                            val elem     : Js.elem
                        end =
struct
  val list_elem = tag0 "div"
  val raw_elem = tag "div" (tag "h2" ($"Links to Raw Data") & list_elem)
  fun addLinks (links:string list) : unit =
      let val hrefs = map (fn s => tag "li" (taga "a" [("href",s)] ($s))) links
          val ul = tag "ul" (connect hrefs)
      in Js.appendChild list_elem ul
      end
  val elem = raw_elem
end


(* The about page *)
structure AboutPage : sig val elem : Js.elem end =
struct
  val elem =
      tag "div"
          (tag "h2" ($"About these Benchmarks") &
               tag "p" ($"The source code for the benchmarks are available from " &
                         taga "a" [("href","https://github.com/melsman/mlkit-bench")]
                         ($"https://github.com/melsman/mlkit-bench") & ($".")))
end

(* The main page *)
val body = case Js.getElementById Js.document "body" of
               SOME e => e
             | NONE => raise Fail "cannot find body element"

val datasetref : (string * Data.line list) list ref = ref nil

val () = navbar body [{pg_title="Snapshot", pg_gen=fn () => SnapshotPage.elem},
                      {pg_title="Historic", pg_gen=fn() => ( HistoricPage.setDataset (!datasetref)
                                                           ; HistoricPage.elem)},
                      {pg_title="Raw Data", pg_gen=fn () => RawDataPage.elem},
                      {pg_title="About", pg_gen=fn () => AboutPage.elem}]


local

(* https://api.github.com/repos/ *)
structure X = Js.XMLHttpRequest

fun getUrl url (f:string->unit) : unit =
    let val r = X.new()
    in X.openn r {method="GET", url=url, async=true}
     ; X.onStateChange r (fn () =>
                             case X.state r of
                                 4 =>
                                 (case X.response r of
                                      SOME s => f s
                                    | NONE => raise Fail ("no response from " ^ url))
                              | _ => ())
     ; X.send r NONE
    end

infix $> ?>
fun (Json.OBJECT obj) ?> s =
    (case Json.objLook obj s of
         SOME t => t
       | NONE => die ("?> couldn't find '" ^ s ^ "' in object"))
  | _ ?> s = die "?> expects left argument to be an object"

fun j $> s =
    case j ?> s of
        Json.STRING s => s
      | _ => die ("$> expects string for '" ^ s ^ "' in object")
in
fun getReports (f : string list -> unit) : unit =
    getUrl "https://api.github.com/repos/melsman/mlkit-bench/contents/reports"
           (fn c =>
               let val l =
                       Json.foldlArrayJson (fn (j,ls) => (j $> "download_url") :: ls) nil c
               in f l
               end)

fun processLink x (f:Data.line list -> unit) : unit =
    getUrl x (fn s => f (Data.fromJsonString s))

fun processLinks links (f:(string*Data.line list) list -> unit) : unit =
    let fun loop xs f =
            case xs of
                nil => f nil
              | x::xs =>
                loop xs (fn ds =>
                            processLink x (fn d => f ((x,d)::ds)))
    in loop links f
    end
end

val () =
    getReports
        (fn links =>
            let val () = RawDataPage.addLinks links
            in processLinks links (fn dataset =>
                                      ( SnapshotPage.setDataset dataset
                                      ; datasetref := dataset)
                                  )
            end)
