
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
              columns:string list,      (* columns to include (all if empty) *)
              skip1:bool,               (* skip first measurement *)
              rsd:bool,                 (* include separate rsd columns *)
              sd:bool,                  (* non-relative stddev *)
              merge_rows:string option} (* merge rows with different provided column but same pname *)

val flags0 : flags = {data=nil,cname=nil,cversion=nil,pname=nil,columns=nil,skip1=false,rsd=false,merge_rows=NONE,sd=false}

fun add_data ({data,cname,cversion,pname,columns,skip1,rsd,sd,merge_rows}:flags) (x:string) : flags =
    {data=x::data,cname=cname,cversion=cversion,pname=pname,columns=columns,skip1=skip1,rsd=rsd,sd=sd,merge_rows=merge_rows}
fun add_cname ({data,cname,cversion,pname,columns,skip1,rsd,sd,merge_rows}:flags) (x:string) : flags =
    {data=data,cname=x::cname,cversion=cversion,pname=pname,columns=columns,skip1=skip1,rsd=rsd,sd=sd,merge_rows=merge_rows}
fun add_cversion ({data,cname,cversion,pname,columns,skip1,rsd,sd,merge_rows}:flags) (x:string) : flags =
    {data=data,cname=cname,cversion=x::cversion,pname=pname,columns=columns,skip1=skip1,rsd=rsd,sd=sd,merge_rows=merge_rows}
fun add_pname ({data,cname,cversion,pname,columns,skip1,rsd,sd,merge_rows}:flags) (x:string) : flags =
    {data=data,cname=cname,cversion=cversion,pname=x::pname,columns=columns,skip1=skip1,rsd=rsd,sd=sd,merge_rows=merge_rows}
fun add_column ({data,cname,cversion,pname,columns,skip1,rsd,sd,merge_rows}:flags) (x:string) : flags =
    {data=data,cname=cname,cversion=cversion,pname=pname,columns=x::columns,skip1=skip1,rsd=rsd,sd=sd,merge_rows=merge_rows}
fun enable_skip1 ({data,cname,cversion,pname,columns,skip1,rsd,sd,merge_rows}:flags) : flags =
    {data=data,cname=cname,cversion=cversion,pname=pname,columns=columns,skip1=true,rsd=rsd,sd=sd,merge_rows=merge_rows}
fun enable_rsd ({data,cname,cversion,pname,columns,skip1,rsd,sd,merge_rows}:flags) : flags =
    {data=data,cname=cname,cversion=cversion,pname=pname,columns=columns,skip1=skip1,rsd=true,sd=sd,merge_rows=merge_rows}
fun enable_sd ({data,cname,cversion,pname,columns,skip1,rsd,sd,merge_rows}:flags) : flags =
    {data=data,cname=cname,cversion=cversion,pname=pname,columns=columns,skip1=skip1,rsd=rsd,sd=true,merge_rows=merge_rows}
fun add_merge_rows ({data,cname,cversion,pname,columns,skip1,rsd,sd,merge_rows}:flags) (x:string): flags =
    {data=data,cname=cname,cversion=cversion,pname=pname,columns=columns,skip1=skip1,rsd=rsd,sd=sd,merge_rows=SOME x}

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
         | "-cversion" => cont add_cversion ss
         | "-data" => cont add_data ss
         | "-column" => cont add_column ss
         | "-skip1" => getCompileArgs(ss,enable_skip1 flags)
         | "-rsd" => getCompileArgs(ss,enable_rsd flags)
         | "-sd" => getCompileArgs(ss,enable_sd flags)
         | "-merge_rows" => cont add_merge_rows ss
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
            | _ => die ("Expecting data specifier to be one of rss, size, data, stk, exe, sys, user, or real - got '" ^ d ^ "'")

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

      fun process (flags:flags) (line:line) : (string*string) list =
          let val runs = #runs line
              val runs = if #skip1 flags then
                           (case runs of _ :: rs => rs | _ => runs)
                         else runs
              fun memory d = List.exists (fn x => x=d) ["rss","size","data","stk","exe"]
              fun pp d v = if memory d then
                             if v >= 100000.0 then real_to_string 0 (v / 1000.0) ^ "M"
                             else if v >= 999.0 then real_to_string 1 (v / 1000.0) ^ "M"
                             else real_to_string 0 v ^ "K"
                           else real_to_string 2 v
              val data = rev(#data flags)
              val cols0 = [("cname",#cname line), ("cversion",#cversion line), ("pname",#pname line)]
              val cols1 = List.map (fn d =>
                                       let val rs = List.map (select d) runs
                                       in case rs of
                                              nil => die (#pname line ^ " " ^ d ^ ": No measurements")
                                            | _ =>
                                              let val (avg,rsd,pct,h,i) =
                                                      if #sd flags
                                                      then let val {avg,sd} = average_sd rs
                                                           in (avg,sd,"","sd",4)
                                                           end
                                                      else let val {avg,rsd} = average_rsd rs
                                                           in (avg,rsd,"%","rsd",1)
                                                           end
                                              in if #rsd flags then
                                                   [(d,pp d avg),(d ^ " " ^ h, real_to_string i rsd)]
                                                 else [(d,pp d avg ^ " ±" ^ real_to_string i rsd ^ pct)]
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
                                                 SOME v => (k,v)::collapse0 (r::rs)
                                               | NONE => die ("groupToCols: no key " ^ k)
          in map collapse tables
          end

      fun pp_cname cn =
          case cn of
              "MLKIT [-gengc]" => "rG"
            | "MLKIT" => "rg"
            | "MLKIT [-no_gc]" => "r"
            | "MLKIT [-no_ri]" => "g"
            | "MLKIT [-no_ri -gengc]" => "G"
            | "MLTON" => "mlton"
            | _ => cn

      fun expands r cn nil = nil
        | expands r cn (k::ks) =
          case find k r of
              SOME v => (k ^ " # " ^ pp_cname cn, v) :: expands r cn ks
            | NONE => expands r cn ks

      fun mergeRows col (table: table) : table =
          groupToCols "pname" (fn r =>
                                  case find col r of
                                      SOME cn => expands r cn ["user","user rsd","rss","rss rsd"]
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
              val rows = List.map (wrap " | " o String.concatWith " | " o List.map #2) table
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
	    ; print "  -pname s      : Filter pname s.\n"
	    ; print "  -cname s      : Filter cname s.\n"
	    ; print "  -cversion s   : Filter cversion s.\n"
	    ; print "  -data s       : s in {data,exe,real,rss,size,stk,sys,user}.\n"
            ; print "  -column c     : Include column c.\n"
            ; print "  -skip1        : Skip the first measument.\n"
            ; print "  -rsd          : Separate columns for relative stddev.\n"
            ; print "  -sd           : Report non-relative stddev.\n"
            ; print "  -merge_rows c : Merge rows with same pname and different c's.\n"
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
