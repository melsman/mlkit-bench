
structure Press = struct

fun die s = raise Fail s
fun println s = print (s ^ "\n")
fun warn s = println ("Warning: " ^ s)

type measurement = DataType.measurement
type line = DataType.line

type flags = {data:string list,         (* name of measurement data entry *)
              cname:string list,        (* compiler name *)
              cversion:string list,     (* compiler version *)
              pname:string list,        (* program name *)
              prune:string list,        (* program name to prune / eliminate *)
              columns:string list,      (* columns to include (all if empty) *)
              bflags:string list,       (* enabled boolean flags *)
              merge_rows:string option} (* merge rows with different provided column but same pname *)

val flags0 : flags = {data=nil,cname=nil,cversion=nil,pname=nil,prune=nil,columns=nil,bflags=nil,merge_rows=NONE}

fun add_data ({data,cname,cversion,pname,prune,columns,bflags,merge_rows}:flags) (x:string) : flags =
    {data=x::data,cname=cname,cversion=cversion,pname=pname,prune=prune,columns=columns,bflags=bflags,merge_rows=merge_rows}
fun add_cname ({data,cname,cversion,pname,prune,columns,bflags,merge_rows}:flags) (x:string) : flags =
    {data=data,cname=x::cname,cversion=cversion,pname=pname,prune=prune,columns=columns,bflags=bflags,merge_rows=merge_rows}
fun add_cversion ({data,cname,cversion,pname,prune,columns,bflags,merge_rows}:flags) (x:string) : flags =
    {data=data,cname=cname,cversion=x::cversion,pname=pname,prune=prune,columns=columns,bflags=bflags,merge_rows=merge_rows}
fun add_pname ({data,cname,cversion,pname,prune,columns,bflags,merge_rows}:flags) (x:string) : flags =
    {data=data,cname=cname,cversion=cversion,pname=x::pname,prune=prune,columns=columns,bflags=bflags,merge_rows=merge_rows}
fun add_prune ({data,cname,cversion,pname,prune,columns,bflags,merge_rows}:flags) (x:string) : flags =
    {data=data,cname=cname,cversion=cversion,pname=pname,prune=x::prune,columns=columns,bflags=bflags,merge_rows=merge_rows}
fun add_column ({data,cname,cversion,pname,prune,columns,bflags,merge_rows}:flags) (x:string) : flags =
    {data=data,cname=cname,cversion=cversion,pname=pname,prune=prune,columns=x::columns,bflags=bflags,merge_rows=merge_rows}

local
  fun enable_bflag f ({data,cname,cversion,pname,prune,columns,bflags,merge_rows}:flags) : flags =
      {data=data,cname=cname,cversion=cversion,pname=pname,prune=prune,columns=columns,bflags=f::bflags,merge_rows=merge_rows}
  fun enabled_bflag f (fs:flags) : bool =
      List.exists (fn s => f = s) (#bflags fs)
  fun enable_enabled f : (flags -> flags) * (flags -> bool) =
      (enable_bflag f, enabled_bflag f)
in
  val (enable_skip1, enabled_skip1) = enable_enabled "skip1"                (* skip first measurement *)
  val (enable_rsd, enabled_rsd) = enable_enabled "rsd"                      (* include separate rsd columns *)
  val (enable_sd, enabled_sd) = enable_enabled "sd"                         (* non-relative stddev *)
  val (enable_M, enabled_M) = enable_enabled "M"
  val (enable_N, enabled_N) = enable_enabled "N"
  val (enable_no_mem_dev, enabled_no_mem_dev) = enable_enabled "no_mem_dev"  (* no deviation data printed for mem-columns *)
  val (enable_latex, enabled_latex) = enable_enabled "latex"                (* latex output *)
  val (enable_gnuplot, enabled_gnuplot) = enable_enabled "gnuplot"          (* gnuplot output *)
  val (enable_shortnames, enabled_shortnames) = enable_enabled "shortnames" (* try to shorten program names *)
end

fun add_merge_rows ({data,cname,cversion,pname,prune,columns,bflags,merge_rows}:flags) (x:string): flags =
    {data=data,cname=cname,cversion=cversion,pname=pname,prune=prune,columns=columns,bflags=bflags,merge_rows=SOME x}

fun sourceFiles nil = nil
  | sourceFiles (input::inputs) =
    case OS.Path.ext input of
        NONE => die ("Missing extension on file '" ^ input ^ "'")
      | SOME "json" => input :: sourceFiles inputs
      | SOME ext => die ("Expecting file with extension 'json' - got file with extension '" ^ ext ^ "'")

fun getCompileArgs (nil, flags:flags) = NONE
  | getCompileArgs (s::ss , flags:flags) =
    let fun cont add ss =
            case ss of
                s::ss => getCompileArgs (ss, add flags s)
	      | nil => die ("Flag " ^ s ^ " expects an argument")
    in case s of
           "-cname" => cont add_cname ss
         | "-pname" => cont add_pname ss
         | "-prune" => cont add_prune ss
         | "-cversion" => cont add_cversion ss
         | "-data" => cont add_data ss
         | "-d" => cont add_data ss
         | "-column" => cont add_column ss
         | "-c" => cont add_column ss
         | "-skip1" => getCompileArgs(ss,enable_skip1 flags)
         | "-rsd" => getCompileArgs(ss,enable_rsd flags)
         | "-sd" => getCompileArgs(ss,enable_sd flags)
         | "-M" => getCompileArgs(ss,enable_M flags)
         | "-N" => getCompileArgs(ss,enable_N flags)
         | "-latex" => getCompileArgs(ss,enable_latex flags)
         | "-gnuplot" => getCompileArgs(ss,enable_gnuplot flags)
         | "-shortnames" => getCompileArgs(ss,enable_shortnames flags)
         | "-sn" => getCompileArgs(ss,enable_shortnames flags)
         | "-merge_rows" => cont add_merge_rows ss
         | "-no_mem_dev" => getCompileArgs(ss,enable_no_mem_dev flags)
         | _ => SOME (sourceFiles(s::ss),flags)
    end

local fun getLines (json_str:string) : line list =
          Data.fromJsonString json_str

      fun cmp (flags:flags) (l1:line,l2:line) : order =
          let fun key (l:line) : string = #pname l ^ "-" ^ #cname l ^ "-" ^ #cversion l
          in String.compare (key l1, key l2)
          end

      fun filter (flags:flags) (l:line) : line option =
          if
            (List.null (#cversion flags) orelse
             List.exists (fn x => x = #cversion l) (#cversion flags))
            andalso
            (List.null (#cname flags) orelse
             List.exists (fn x => x = #cname l) (#cname flags))
            andalso
            (List.null (#pname flags) orelse
             List.exists (fn x => x = #pname l) (#pname flags))
            andalso
            (not(List.exists (fn x => x = #pname l) (#prune flags)))
          then SOME l
          else NONE

      fun select d (m:measurement) : real =
          case d of
              "rss" => real(#rss m)
            | "size" => real(#size m)
            | "data" => real(#data m)
            | "stk" => real(#stk m)
            | "exe" => real(#exe m)
            | "sys" => #sys m
            | "user" => #user m
            | "real" => #real m
            | "gc" => #gc m
            | "majgc" => #majgc m
            | "gcn" => real(#gcn m)
            | "majgcn" => real(#majgcn m)
            | _ => die ("Expecting data specifier to be one of rss, size, data, stk, exe, sys, user, real, gc, gcn, majgc, or majgcn - got '" ^ d ^ "'")

      fun is_time_col d =
          case d of
              "sys" => true
            | "user" => true
            | "real" => true
            | "gc" => true
            | "majgc" => true
            | _ => false

      fun sq s : real = s * s
      fun wrap s e = s ^ e ^ s

      fun transpose (ss:'a list list) : 'a list list =
          if List.all (List.null) ss then nil
          else let val ccs =
                       List.map (fn r =>
                                    case r of h::t => (h,t)
                                            | nil => die "transpose: non-regular array") ss
                   val c = List.map #1 ccs
                   val rest = List.map #2 ccs
               in c :: transpose rest
               end

      fun max (a,b) = if a > b then a else b

      fun mysize s =
          if String.isSubstring "±" s then size s - 1
          else size s

      fun mypadRight c m v =
          if String.isSubstring "±" v then StringCvt.padRight c (m+1) v
          else StringCvt.padRight c m v

      fun pad (col_values:(string*string) list) : (string*string) list =
          let val m = List.foldl max 0 (List.map (mysize o #2) col_values)
                                 (* pad left for numbers *)
              val p =
                  if List.all (CharVector.all (fn c => Char.isDigit c orelse c = #"%" orelse c = #".") o #2) col_values
                  then StringCvt.padLeft
                  else mypadRight
          in List.map (fn (k,v) => (k,p #" " m v)) col_values
          end

      fun pads (flags:flags) rows =
          let val cols = transpose rows
              val cols = List.map pad cols
              val cols =
                  case rev(#columns flags) of
                      nil => cols
                    | colnames => List.mapPartial (fn c => List.find (fn ((k,_)::_) => k=c | _ => false) cols)
                                                  colnames
          in transpose cols
          end

      fun real_to_string n r =
          Real.fmt (StringCvt.FIX(SOME n)) r

      fun average (xs:real list) : real =
          case xs of
              nil => 0.0
            | _ => List.foldl (op +) 0.0 xs / (real(length xs))

      fun average_sd (xs:real list) : {avg:real,sd:real} =
          let val avg = average xs
          in case xs of
                 nil => {avg=0.0,sd=0.0}
               | [x] => {avg=avg,sd=0.0}
               | _ => {avg=avg,
                       sd=Math.sqrt (List.foldl (op +) 0.0 (List.map (fn x => sq(x-avg)) xs) / (real(length xs - 1)))}
          end

      fun average_rsd (xs:real list) : {avg:real,rsd:real} =
          let val {avg,sd} = average_sd xs
          in if Real.==(0.0,avg) then {avg=avg,rsd=0.0}
             else {avg=avg,rsd=100.0*sd/avg}
          end

      fun pp_tt flags s =
          if enabled_gnuplot flags then
            "\"" ^ s ^ "\""
          else if enabled_latex flags then "\\mbox{\\tt " ^ s ^ "}"
          else s

      fun strip_ext s =
          case String.tokens (fn c => c = #".") s of
              [f,"sml"] => f
            | [f,"mlb"] => f
            | _ => s

      fun strip_dir s =
          case rev(String.tokens (fn c => c = #"/") s) of
              f :: _ => f
            | _ => s

      fun pp_pname flags s =
          let val s = if enabled_shortnames flags then
                        strip_dir(strip_ext s)
                      else s
          in if enabled_latex flags orelse enabled_gnuplot flags then
               pp_tt flags (strip_ext s)
             else s
          end

      fun process (flags:flags) (line:line) : (string*string) list =
          let val runs = #runs line
              val runs = if enabled_skip1 flags then
                           (case runs of _ :: rs => rs | _ => runs)
                         else runs
              fun is_memory d = List.exists (fn x => x=d) ["rss","size","data","stk","exe"]
              fun is_count d = List.exists (fn x => x=d) ["gcn","majgcn"]
              fun pp d v = if is_memory d then
                             if enabled_M flags then
                               real_to_string 0 (v / 1000.0)
                             else if v >= 100000.0 then real_to_string 0 (v / 1000.0) ^ "M"
                             else if v >= 999.0 then real_to_string 1 (v / 1000.0) ^ "M"
                             else real_to_string 0 v ^ "K"
                           else if is_count d andalso enabled_N flags then
                             real_to_string 0 v
                           else real_to_string 2 v
              val data = rev(#data flags)
              val cols0 = [("cname",#cname line), ("cversion",#cversion line), ("pname",pp_pname flags(#pname line)),
                           ("plen",Int.toString(#plen line))]
              val (pct_s,pm_s) = if enabled_latex flags then ("\\%", "\\pm ") else ("%"," ±")
              val cols1 = List.map (fn d =>
                                       let val rs = List.map (select d) runs
                                       in case rs of
                                              nil => die (#pname line ^ " " ^ d ^ ": No measurements")
                                            | _ =>
                                              let val (avg,rsd,pct,h,i) =
                                                      if enabled_sd flags
                                                      then let val {avg,sd} = average_sd rs
                                                           in (avg,sd,"","sd",4)
                                                           end
                                                      else let val {avg,rsd} = average_rsd rs
                                                           in (avg,rsd,pct_s,"rsd",0)
                                                           end
                                              in if enabled_rsd flags then
                                                   [(d,pp d avg),(d ^ " " ^ h, real_to_string i rsd)]
                                                 else if enabled_no_mem_dev flags andalso not (is_time_col d) then
                                                   [(d,pp d avg)]
                                                 else
                                                   [(d,pp d avg ^ pm_s ^ real_to_string i rsd ^ pct)]
                                              end
                                       end) data
          in cols0 @ List.concat cols1
          end

      type row = (string * string) list
      type table = row list

      fun find (k:string) (kvs:row) : string option =
          Option.map (#2) (List.find (fn (x,_) => k = x) kvs)

      fun groupBy (k:string) (table: table) : table list =
          let fun loop (table:table) (acc:table) (res:table list) : table list =
                  case (table,acc) of
                      (row::table,nil) => loop table [row] res
                    | (nil,nil) => rev res
                    | (nil,_) => rev(rev acc::res)
                    | (row::table,r::acc) =>
                      if find k row = find k r then loop table (row::r::acc) res
                      else loop table [row] (rev(r::acc)::res)
          in loop table nil nil
          end

      fun groupToCols (k:string) (expand: row -> row) (table: table) : table =
          let val tables = groupBy k table
              fun collapse0 nil : row = nil
                | collapse0 (r::rs) : row = expand r @ collapse0 rs
              fun collapse nil : row = nil
                | collapse ((r::rs):table) = case find k r of
                                                 SOME v =>
                                                 let val res = collapse0 (r::rs)
                                                 in case find "plen" r of
                                                        SOME l => (k,v)::("plen", l)::res
                                                      | NONE => (k,v)::res
                                                 end
                                               | NONE => die ("groupToCols: no key " ^ k)
          in map collapse tables
          end

      fun pp_cname cn =
          case cn of
              "MLKIT [-gengc]" => "rG"
            | "MLKIT" => "rg"
            | "MLKIT [-cr]" => "rg-cr"
            | "MLKIT [-disable_spurious_type_variables -scratch]" => "rg-"
            | "MLKIT [-no_gc]" => "r"
            | "MLKIT [-no_ri]" => "g"
            | "MLKIT [-no_ri -gengc]" => "G"
            | "MLTON" => "mlton"
            | "SMLNJ" => "smlnj"
            | "MLTON [-mlb-path-var 'MLCOMP mlton']" => "mlton"
            | "MLKIT [MLCOMP=mlkit-seq -no_gc]" => "seq"
            | "MLKIT [MLCOMP=mlkit-seq -no_gc -par -mlb-subdir C1]" => "par1"
            | "MLKIT [MLCOMP=mlkit-par -no_gc -par]" => "par"
            | "MLKIT [-no_gc -no_high_pointer_tagging -mlb-subdir C1]" => "r-nhpt"
            | "MLKIT [-no_high_pointer_tagging -mlb-subdir C1]" => "rg-nhpt"
            | "MLKIT [-no_high_pointer_tagging -mlb-subdir C1 -cr]" => "rg-nhpt-cr"
            | _ =>
              if String.isPrefix "MPL" cn
              then "mpl"
              else if String.isPrefix "MLKIT" cn andalso String.isSubstring "-argo" cn
              then "argo"
              else cn

      fun expands r cn nil = nil
        | expands r cn (k::ks) =
          case find k r of
              SOME v => (k ^ " # " ^ pp_cname cn, v) :: expands r cn ks
            | NONE => expands r cn ks

      fun mergeRows col (table: table) : table =
          groupToCols "pname" (fn r =>
                                  case find col r of
                                      SOME cn => expands r cn ["user","user rsd","user sd",
                                                               "real","real rsd","real sd",
                                                               "gc","gc rsd","gc sd",
                                                               "majgc","majgc rsd","majgc sd",
                                                               "gcn",
                                                               "majgcn",
                                                               "rss","rss rsd","rss sd"
                                                              ]
                                    | NONE => nil) table

      fun processAll flags (lines: line list) : unit =
          let val table = List.map (process flags) lines
              val table = case #merge_rows flags of
                              NONE => table
                            | SOME col => mergeRows col table
              val table = (* add header *)
                  case table of
                      row::_ => let val hs = List.map (fn (k,_) => (k,k)) row
                                in hs::table
                                end
                    | _ => table
              val table = pads flags table
              val (wr, sep) =
                  if enabled_latex flags then
                    (fn s => s ^ " \\\\ ", " & ")
                  else if enabled_gnuplot flags then
                    (fn s => s, "   ")
                  else (wrap " | ", " | ")
              val rows = List.map (wr o String.concatWith sep o List.map #2) table
          in List.app println rows
          end
in

fun main (progname, args) =
    let
(*
	val _ = print "Args: ["
	val _ = app (fn s => print (s ^ ",")) args
	val _ = print "]\n"
*)
    in case getCompileArgs (args, flags0) of
           NONE =>
	   (  print "USAGE: mlkit-bench-press [OPTION]... FILE...\n"
	    ; print "OPTIONS:\n"
	    ; print "  -pname s        : Include pname s.\n"
	    ; print "  -prune s        : Exclude pname s.\n"
	    ; print "  -cname s        : Filter cname s.\n"
	    ; print "  -cversion s     : Filter cversion s.\n"
	    ; print "  -data s, -d s   : s in {data,exe,real,rss,size,stk,sys,user,gc,majgc,gcn,majgcn}.\n"
            ; print "  -column c,-c c  : Include column c.\n"
            ; print "  -skip1          : Skip the first measument.\n"
            ; print "  -rsd            : Separate columns for relative stddev.\n"
            ; print "  -sd             : Report non-relative stddev.\n"
            ; print "  -M              : Report memusage in Mb without extension.\n"
            ; print "  -N              : Report counts without decimal places.\n"
            ; print "  -latex          : LaTeX output.\n"
            ; print "  -gnuplot        : Gnuplot output.\n"
            ; print "  -merge_rows c   : Merge rows with same pname and different c's.\n"
            ; print "  -shortnames,-sn : Try to shorten program names.\n"
	    ; print "\n"
	    ; print "FILES:\n"
	    ; print "  file.json    : Json file.\n"
	    ; print "\n"
	    ; print "EXAMPLE:\n"
	    ; print "  The command\n"
            ; print "    $ mlkit-bench-press -cversion 'MLKit v4.5.1' -pname life.sml -data real report.json\n"
            ; print "  will output wall clock timings for the benchmark 'life.sml'.\n"
	    ; print "\n"
	    ; OS.Process.failure)
	 | SOME (inputs, flags) =>
	   let val json_strs = List.map FileUtil.readFile inputs
               val lines = List.concat (List.map getLines json_strs)
               val lines = List.mapPartial (filter flags) lines
               val lines = Listsort.sort (cmp flags) lines
           in processAll flags lines
	    ; OS.Process.success
	   end
    end handle Fail s =>
               ( println ("ERROR: " ^ s)
               ; OS.Process.failure)
end

end

structure Main : sig end = struct
val res = Press.main (CommandLine.name(), CommandLine.arguments())
val _ = OS.Process.exit res
end
