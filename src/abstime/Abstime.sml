
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
                        a::ss => getCompileArgs (ss, a::jsons)
                      | _ => NONE)
      | _ =>
        SOME (s::ss,rev jsons)

local
fun getLines (json_str:string) : line list =
    Data.fromJsonString json_str

fun getBenchLine bench [] = raise Fail ("Missing data for benchmark: " ^ bench)
  | getBenchLine bench ((l:line)::ls) =
    if #pname l = bench then l else getBenchLine bench ls

fun meanrun (l: line) =
    foldl op+ 0.0 (map #real (#runs l)) / real (length (#runs l))

fun process (lines: line list list) (benchmark: string) : string =
    let val b = "\\texttt{" ^ OS.Path.base (OS.Path.file benchmark) ^ "}"
        fun f a =
            let val l = getBenchLine benchmark a
                val x = meanrun l
            in "$" ^ Real.fmt (StringCvt.FIX (SOME 3)) x ^ "s$"
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
                   List.map (getLines o FileUtil.readFile) jsons
           in print (String.concatWith sep (map (process measurements) benchmarks) ^ sep);
              OS.Process.success
           end
         | _ =>
	   (  print "USAGE: mlkit-bench-abstime [OPTIONS] benchmarks...\n"
	    ; print "OPTIONS:\n"
	    ; print "  -json FILE.json : Measurements in these files.\n"
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
