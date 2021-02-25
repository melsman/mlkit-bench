
structure Speedup = struct

fun die s = raise Fail s
fun println s = print (s ^ "\n")
fun warn s = println ("Warning: " ^ s)

type measurement = DataType.measurement
type line = DataType.line

type settings =
     {jsons:(int * string) list, (* JSONs with measurements *)
      baseline:string option     (* JSON with baseline measurements *)
     }

val settings0 : settings = {jsons=nil,baseline=NONE}

fun add_measurement n ({jsons, baseline}:settings) json : settings =
    {jsons=(n,json)::jsons,
     baseline=baseline}

fun add_baseline ({jsons, baseline=_}:settings) baseline : settings =
    {jsons=jsons,
     baseline=SOME baseline}

fun getCompileArgs (nil, settings : settings) = NONE
  | getCompileArgs (s::ss , settings : settings) =
    let fun cont add ss =
            case ss of
                s::ss => getCompileArgs (ss, add settings s)
	      | nil => die ("Option " ^ s ^ " expects an argument")
    in case s of
           "-baseline" => cont add_baseline ss
         | _ =>
           (if String.isPrefix "-" s
            then case Int.fromString (String.extract(s,1,NONE)) of
                     SOME x => cont (add_measurement x) ss
                   | NONE => die ("Unknown option: " ^ s)
            else SOME (s::ss,settings))
    end

local
fun getLines (json_str:string) : line list =
    Data.fromJsonString json_str

fun getBenchLine bench [] = raise Fail ("Missing data for benchmark: " ^ bench)
  | getBenchLine bench ((l:line)::ls) =
    if #pname l = bench then l else getBenchLine bench ls

fun meanrun (l: line) =
    foldl op+ 0.0 (map #real (#runs l)) / real (length (#runs l))

fun process benchmarks (baselines: line list) (n, measurements: line list) : string =
    let fun process1 benchmark =
            let val x = getBenchLine benchmark baselines
                val y = getBenchLine benchmark measurements
            in Real.toString (meanrun x / meanrun y)
            end
    in String.concatWith " "  (Int.toString n :: map process1 benchmarks) ^ "\n"
    end
in

fun main (progname, args) =
    let
    in case getCompileArgs (args, settings0) of
	   SOME (benchmarks, {jsons, baseline=SOME baseline}) =>
 	   let val measurements =
                   rev ((List.map (fn (n,f) => (n, getLines (FileUtil.readFile f)))
                                  jsons))
               val baselines = getLines (FileUtil.readFile baseline)
           in print(String.concat (map (process benchmarks baselines) measurements));
	      OS.Process.success
	   end
         | _ =>
	   (  print "USAGE: mlkit-bench-speedup [OPTIONS] benchmarks...\n"
	    ; print "OPTIONS:\n"
	    ; print "  -baseline FILE  : Use this file as baseline.\n"
	    ; print "  -N FILE         : N must be an integer; use runtimes from FILE.\n"
	    ; print "\n"
	    ; print "FILES:\n"
	    ; print "  file.json    : Json file.\n"
	    ; print "\n"
	    ; OS.Process.failure)

    end handle Fail s =>
               ( println ("ERROR: " ^ s)
               ; OS.Process.failure)
end

end

structure Main : sig end = struct
val res = Speedup.main (CommandLine.name(), CommandLine.arguments())
val _ = OS.Process.exit res
end
