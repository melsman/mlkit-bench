
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

fun speedupOf (benchmark: string) (baselines: line list) (measurements: line list) : real =
    let val x = getBenchLine benchmark baselines
        val y = getBenchLine benchmark measurements
    in meanrun x / meanrun y
    end

fun process (benchmarks: string list) (baselines: line list) (n, measurements: line list) : string =
    let fun process1 benchmark = Real.toString (speedupOf benchmark baselines measurements)
    in String.concatWith " "  (Int.toString n :: map process1 benchmarks) ^ "\n"
    end

  local
    fun merge cmp ([], ys) = ys
      | merge cmp (xs, []) = xs
      | merge cmp (xs as x::xs', ys as y::ys') =
          case cmp (x, y) of
               GREATER => y :: merge cmp (xs, ys')
             | _       => x :: merge cmp (xs', ys)
    fun sort cmp [] = []
      | sort cmp [x] = [x]
      | sort cmp xs =
        let
          val ys = List.take (xs, length xs div 2)
          val zs = List.drop (xs, length xs div 2)
        in
          merge cmp (sort cmp ys, sort cmp zs)
        end
    fun lastSpeedup benchmark baselines (measurements: (int * line list) list) : real =
        speedupOf benchmark baselines (#2 (List.last measurements))
  in
    fun sortBenchmarks benchmarks baselines measurements =
        let fun cmp ((_, x), (_, y)) = Real.compare (x, y)
        in
          (rev o map #1 o sort cmp o map (fn b => (b, lastSpeedup b baselines measurements))) benchmarks
        end
  end
in

fun main (progname, args) =
    let
    in case getCompileArgs (args, settings0) of
	   SOME (benchmarks, {jsons, baseline=SOME baseline}) =>
 	   let val measurements =
                   List.map (fn (n,f) => (n, getLines (FileUtil.readFile f))) (rev jsons)
               val baselines = getLines (FileUtil.readFile baseline)
               val benchName = OS.Path.base o OS.Path.file
               val benchmarks = sortBenchmarks benchmarks baselines measurements
           in print(String.concatWith " " ("n" :: map benchName benchmarks) ^ "\n");
              print(String.concat (map (process benchmarks baselines) measurements));
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
