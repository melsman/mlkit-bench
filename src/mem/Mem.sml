
structure Speedup = struct

fun die s = raise Fail s
fun println s = print (s ^ "\n")
fun warn s = println ("Warning: " ^ s)

type measurement = DataType.measurement
type line = DataType.line

fun getCompileArgs (nil, jsons) = NONE
  | getCompileArgs (s::ss , jsons) =
    case s of
        "-json" => (case ss of
                        s::ss => getCompileArgs (ss, (String.tokens (fn c => c = #",") s)::jsons)
                      | _ => NONE)
      | _ =>
        SOME (s::ss,rev jsons)

local
fun getLines (json_str:string) : line list =
    Data.fromJsonString json_str

fun getBenchLine bench [] = raise Fail ("Missing data for benchmark: " ^ bench)
  | getBenchLine bench ((l:line)::ls) =
    if #pname l = bench then l else getBenchLine bench ls

fun meanrss (l: line) =
    round (foldl op+ 0.0 (map (real o #rss) (#runs l)) / real (length (#runs l)))

fun process (lines: line list list list) (benchmark: string) : string =
    let val b = "\\texttt{" ^ OS.Path.base (OS.Path.file benchmark) ^ "}"
        fun f [] = raise Fail "No JSONs"
          | f (a::bs) =
            let val a_l = getBenchLine benchmark a
                val bs_l = List.map (getBenchLine benchmark) bs
                val a_rss = meanrss a_l
                val bs_rss = List.map meanrss bs_l
                fun rel b_rss =
                    " & $" ^ Real.fmt (StringCvt.FIX (SOME 2)) (real b_rss / real a_rss) ^ "\\times$"
                val baseline =
                    if a_rss < 1024
                    then Int.toString a_rss ^ "KiB"
                    else Int.toString (a_rss div 1024) ^ "MiB"
            in concat (baseline :: map rel bs_rss)
            end
    in String.concatWith " & " (b  :: map f lines)
    end
in

fun main (progname, args) =
    let
    in case getCompileArgs (args, nil) of
	   SOME (benchmarks, jsons) =>
           let val sep = "\\\\\n"
               val measurements =
                   List.map (List.map (getLines o FileUtil.readFile)) jsons
           in print (String.concatWith sep (map (process measurements) benchmarks) ^ sep);
              OS.Process.success
           end
         | _ =>
	   (  print "USAGE: mlkit-bench-mem [OPTIONS] benchmarks...\n"
	    ; print "OPTIONS:\n"
	    ; print "  -json FILE1.json,FILE2.json,...FILE3.json : Compare these files.\n"
	    ; print "\n"
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
