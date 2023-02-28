structure CompileMLKIT : COMPILE = struct

fun die s = (print ("ERROR: " ^ s ^ "\n"); raise Fail s)
fun mlkit_root () =
    case OS.Process.getEnv "MLKIT_ROOT" of
        SOME r => r
      | NONE => die "Environment variable MLKIT_ROOT not set"
fun compact s =
    let fun f(nil,acc) = implode(rev acc)
	  | f(#" "::cs,acc) = f (cs,acc)
          | f(#","::cs,acc) = f (cs,#"-"::acc)
          | f(#"/"::cs,acc) = f (cs,#"-"::acc)
          | f(#"."::cs,acc) = f (cs,#"-"::acc)
	  | f(c::cs,acc) = f (cs, c::acc)
    in f (explode s, nil)
    end

fun mlkit_cmd () =
    let val root = mlkit_root()
    in "SML_LIB=" ^ root ^ " " ^ root ^ "/bin/mlkit"
    end

fun compile {env:(string*string)list,flags:string, src:string} =
    let val {base,ext} = OS.Path.splitBaseExt src
	val t = base ^ "_mlkit" ^ compact flags ^ ".exe"
        val cmd = mlkit_cmd() ^ " " ^ flags ^ " -o " ^ t ^ " " ^ src
        val env = String.concatWith " " (map (fn (k,v) => k ^ "=" ^ v) env)
        val cmd = env ^ " " ^ cmd
    in  print ("Executing: '" ^ cmd ^ "'\n")
      ; if OS.Process.isSuccess(OS.Process.system cmd) then SOME t
	else NONE
    end

fun version () =
    let val cmd = mlkit_cmd() ^ " -version"
    in FileUtil.trimWS(FileUtil.systemOut cmd)
    end handle X => ("Failed to extract mlkit version; " ^ exnMessage X)

end
