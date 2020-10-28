
structure Press = struct

fun die s = raise Fail s
fun println s = print (s ^ "\n")
fun warn s = println ("Warning: " ^ s)

type measurement = DataType.measurement
type line = DataType.line

type flags = {data:string list,        (* name of measurement data entry *)
              cname:string list,       (* compiler name *)
              cversion:string list,    (* compiler version *)
              pname:string list,       (* program name *)
              prune:string list}       (* columns to prune if unique *)

val flags0 : flags = {data=nil,cname=nil,cversion=nil,pname=nil,prune=nil}

fun add_data ({data,cname,cversion,pname,prune}:flags) (x:string) : flags =
    {data=x::data,cname=cname,cversion=cversion,pname=pname,prune=prune}
fun add_cname ({data,cname,cversion,pname,prune}:flags) (x:string) : flags =
    {data=data,cname=x::cname,cversion=cversion,pname=pname,prune=prune}
fun add_cversion ({data,cname,cversion,pname,prune}:flags) (x:string) : flags =
    {data=data,cname=cname,cversion=x::cversion,pname=pname,prune=prune}
fun add_pname ({data,cname,cversion,pname,prune}:flags) (x:string) : flags =
    {data=data,cname=cname,cversion=cversion,pname=x::pname,prune=prune}
fun add_prune ({data,cname,cversion,pname,prune}:flags) (x:string) : flags =
    {data=data,cname=cname,cversion=cversion,pname=pname,prune=x::prune}

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
         | "-prune" => cont add_prune ss
         | _ => SOME (sourceFiles(s::ss),flags)
    end

local fun getLines (json_str:string) : line list =
          Data.fromJsonString json_str

      fun cmp (flags:flags) (l1:line,l2:line) : order =
          let fun key (l:line) : string = #cname l ^ #cversion l ^ #pname l
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

      fun pad (col_values:(string*string) list) : (string*string) list =
          let val m = List.foldl max 0 (List.map (size o #2) col_values)
                                 (* pad left for numbers *)
              val p =
                  if List.all (CharVector.all (fn c => Char.isDigit c orelse c = #"%" orelse c = #".") o #2) col_values
                  then StringCvt.padLeft
                  else StringCvt.padRight
          in List.map (fn (k,v) => (k,p #" " m v)) col_values
          end

      fun pads (flags:flags) rows =
          let val cols = transpose rows
              val cols = List.map pad cols
              val cols = List.mapPartial (fn (kvs as (_::(k,v)::_)) =>
                                             if List.exists (fn x => k=x) (#prune flags) then
                                               if List.all (fn (_,v') => v=v' orelse String.isPrefix k v') kvs then NONE
                                               else (warn ("Deleting column '" ^ k ^ "' (values differ)");
                                                     NONE)
                                             else SOME kvs
                                           | kvs => SOME kvs) cols
          in transpose cols
          end

      fun real_to_string n r =
          Real.fmt (StringCvt.FIX(SOME n)) r

      fun process (flags:flags) (line:line) : (string*string) list list =
          let val runs = #runs line
              val data = #data flags
          in List.map (fn d =>
                               let val rs = List.map (select d) runs
                               in case length rs of
                                      0 => die (#pname line ^ " " ^ d ^ ": No measurements")
                                    | len =>
                                      let val avg = List.foldl (op +) 0.0 rs / (real len)
                                          val sd = if len = 0 then 0.0
                                                   else Math.sqrt (List.foldl (op +) 0.0 (List.map (fn x => sq(x-avg)) rs) / (real(len - 1)))
                                          val rsd = sd * 100.0 / avg
                                      in [("cname",#cname line), ("cversion",#cversion line), ("pname",#pname line), ("data",d),
                                          (d,real_to_string 3 avg), ("rsd%",real_to_string 2 rsd)]
                                      end
                               end) data
          end

      fun processAll flags (lines: line list) : unit =
          let val sss = List.concat (List.map (process flags) lines)
              val sss =
                  case sss of
                      ss::_ => let val hs = List.map (fn (k,_) => (k,k)) ss
                               in hs::sss
                               end
                    | _ => sss
              val sss = pads flags sss
              val ss = List.map (wrap " | " o String.concatWith " | " o List.map #2) sss
          in List.app println ss
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
	    ; print "  -pname s     : Filter pname s.\n"
	    ; print "  -cname s     : Filter cname s.\n"
	    ; print "  -cversion s  : Filter cname s.\n"
	    ; print "  -data s      : s in {data,exe,real,rss,size,stk,sys,user}.\n"
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
