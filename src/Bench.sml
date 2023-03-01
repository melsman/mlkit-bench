
structure Bench = struct

datatype compiler = MLKIT of string | MLTON of string | MPL of string
fun pr_compiler (c:compiler): string =
    let fun with_flags "" s = s
	  | with_flags flags s = s ^ " [" ^ flags ^ "]"
    in case c of
           MLKIT flags => with_flags flags "MLKIT"
         | MLTON flags => with_flags flags "MLTON"
         | MPL flags => with_flags flags "MPL"
    end


type measurement = MemTime.measurement

fun files_equal (s1,s2) =
    let fun open_file s = TextIO.openIn s
	val is1 = open_file s1
	val is2 = open_file s2
	fun close() = (TextIO.closeIn is1; TextIO.closeIn is2)
    in (TextIO.inputAll(is1) = TextIO.inputAll(is2) before (close()))
    end handle _ => false

fun exec {cmd,args,out_file} : measurement =
    MemTime.memTime {cmd=cmd,args=args,out_file=out_file, eout_file=NONE}

fun exec' {cmd,args,out_file} : measurement list =
    MemTime.memTime' {cmd=cmd,args=args,out_file=out_file, eout_file=NONE}

fun exec_n n {out_file,cmd} =
    let val L = List.tabulate (n,fn i => i+1)
        val args =
            if String.isSubstring "mlkit" cmd andalso
               not(String.isSubstring "-no_gc" cmd)
            then ["-report_gc"]
            else []
        val () = print ("Executing: " ^ cmd ^ " " ^ String.concatWith " " args ^ " - ")
        val R = List.map (fn i =>
                             (print (Int.toString i ^ ":");
                              exec {cmd=cmd,
                                    args=args,
                                    out_file=out_file i})) L
        val () = print "\n"
    in R
    end

fun exec_n' n {out_file,cmd} =
    let val args = ["-r", Int.toString n, "--it"]
        val () = print ("Executing: " ^ cmd ^ " " ^ String.concatWith " " args ^ " - ")
        val ms = exec' {cmd=cmd,args=args,out_file=out_file 1}
        val () = print "done]\n"
    in ms
    end

val repetitions = ref 10
val internal_timings = ref false

fun process (compile: string -> string option * Time.time) (p:string)
    : string * Time.time * measurement list =
    case compile p of
        (SOME t,ctime) =>
	let val out = t ^ ".out.1"  (* memo: we could check every invocation *)
            val cmd = t
            val the_exec_n =
                if !internal_timings then exec_n'
                else exec_n
	    val res = (t, ctime,
                       the_exec_n (!repetitions)
                                  {out_file=fn i => t ^ ".out." ^ Int.toString i,
                                   cmd=cmd})
	              handle Fail s => raise Fail ("Failure executing command '" ^ cmd ^ "': " ^ s)
                           | X => raise Fail ("Failure executing command '" ^ cmd ^ "': " ^ General.exnMessage X)
        in if files_equal(p ^ ".out.ok", out) then res
           else raise Fail ("File " ^ p ^ ".out.ok does not match " ^ out)
	end
      | (NONE,_) => raise Fail "Compile error"

fun readFlags ss =
    let fun concatWith d (nil) = ""
	  | concatWith d [s] = s
	  | concatWith d (s::ss) = s ^ d ^ concatWith d ss
	fun readFls (ss,acc) =
	    case ss of
                ":"::ss => SOME (concatWith " " (rev acc),ss)
	      | s::ss => readFls(ss,s::acc)
	      | _ => NONE
    in case ss of
           ":"::ss => readFls (ss,nil)
	 | _ => NONE
    end

fun getCompileArgs (nil, comps, out) = NONE
  | getCompileArgs (s::ss , comps, out) =
    case s of
        "-mlkit"   =>
        (case readFlags ss of
             SOME (flags,ss) => getCompileArgs (ss, MLKIT flags::comps, out)
	   | NONE => getCompileArgs (ss, MLKIT "" ::comps, out))
      | "-mlton"   =>
	(case readFlags ss of
             SOME (flags,ss) => getCompileArgs (ss, MLTON flags::comps, out)
	   | NONE => getCompileArgs (ss, MLTON "" :: comps, out))
      | "-mpl"   =>
	(case readFlags ss of
             SOME (flags,ss) => getCompileArgs (ss, MPL flags::comps, out)
	   | NONE => getCompileArgs (ss, MPL "" :: comps, out))
      | "-o" =>
	(case ss of
             f::ss => getCompileArgs (ss, comps, SOME f)
	   | _ => NONE)
      | "-r" =>
        (case ss of
             s::ss =>
             (case Int.fromString s of
                  SOME n =>
                  if n > 0 then ( repetitions := n
                                ; getCompileArgs (ss, comps, out))
                  else NONE
                | NONE => NONE)
           | _ => NONE)
      | "-it" => ( internal_timings := true; getCompileArgs (ss,comps,out) )
      | _ => SOME (s::ss, rev comps, out)

fun splitFlags (flags:string) : {env:(string*string)list,flags:string} =
    let val ss = String.tokens (fn #" " => true | _ => false) flags
        fun split (nil, env, flags) =
            {env=rev env, flags=String.concatWith " " (rev flags)}
          | split (x::xs, env, flags) =
            case String.tokens (fn #"=" => true | _ => false) x of
                [k,v] => split (xs,(k,v)::env,flags)
              | [_] => split(xs,env,x::flags)
              | _ => raise Fail "expecting at most one '=' character in flag"
    in split (ss,nil,nil)
    end

fun getNameComp c =
    let val head = pr_compiler c
    in case c of
           MLKIT flags => {head=head, compile=CompileMLKIT.compile,
                           flags=flags, cversion=CompileMLKIT.version()}
	 | MLTON flags => {head=head, compile=CompileMLTON.compile,
                           flags=flags, cversion=CompileMLTON.version()}
	 | MPL flags => {head=head, compile=CompileMPL.compile,
                         flags=flags, cversion=CompileMPL.version()}
    end

fun sourceFiles nil = nil
  | sourceFiles (input::inputs) =
    case OS.Path.ext input of
        NONE => raise Fail ("Missing extension on file " ^ input)
      | SOME "sml" => input :: sourceFiles inputs
      | SOME "mlb" => input :: sourceFiles inputs
      | SOME ext => raise Fail ("Unknown extension " ^ ext)

type line = DataType.line

local open Json
      val pr_r = Time.toString o Time.fromReal
in

fun runToJson ({rss, size, data, stk, exe,
	        sys, user, real,
                gc, gcn, majgc, majgcn} : measurement) : Json.obj =
    objFromList [("rss",NUMBER(Int.toString rss)),
                 ("size",NUMBER(Int.toString size)),
                 ("data",NUMBER(Int.toString data)),
                 ("stk",NUMBER(Int.toString stk)),
                 ("exe",NUMBER(Int.toString exe)),
                 ("sys",NUMBER(pr_r sys)),
                 ("user",NUMBER(pr_r user)),
                 ("real",NUMBER(pr_r real)),
                 ("gc",NUMBER(pr_r gc)),
                 ("majgc",NUMBER(pr_r majgc)),
                 ("gcn",NUMBER(Int.toString gcn)),
                 ("majgcn",NUMBER(Int.toString majgcn))
                ]

fun lineToJson ({cname,cversion,date,mach,pname,
                 plen,ctime,binsz,runs,err}:line) : Json.obj =
    let val runs = map (OBJECT o runToJson) runs
    in objFromList [("cname",STRING cname),
                    ("cversion",STRING cversion),
                    ("datetime", STRING (Date.fmt "%Y-%m-%d %H:%M" date)),
                    ("mach", STRING mach),
                    ("pname", STRING pname),
                    ("plen", NUMBER (Int.toString plen)),
                    ("ctime", NUMBER (pr_r ctime)),
                    ("binsz", NUMBER (Int.toString binsz)),
                    ("runs", ARRAY runs),
                    ("err", STRING err)]
    end

fun toJson (lines:line list) : Json.t = ARRAY (map (OBJECT o lineToJson) lines)
end

fun timewrap f x =
    let val t = Time.now()
        val r = f x
        val t' = Time.now()
    in (r, Time.-(t',t))
    end

fun getMachine () =
    let fun sysOut1 s = FileUtil.trimWS(FileUtil.systemOut s)
        val uname = sysOut1 "uname -srm"
    in if String.isSubstring "Darwin" uname then
         uname ^ " - " ^ sysOut1 "sysctl -n machdep.cpu.brand_string"
       else uname
    end

val today = Date.fromTimeLocal(Time.now())
val machine = getMachine()

fun process_progs ps c : line list =
    let val {head,compile,cversion,flags} = getNameComp c
        val {env,flags} = splitFlags flags
	val _ = print ("[Processing benchmark programs for " ^ head ^ "]\n")
        fun process_prog p : line =
            let val (outfile, ctime, runs) =
                    process (fn s => timewrap compile {env=env,flags=flags,src=s}) p
                val binsz = Posix.FileSys.ST.size (Posix.FileSys.stat outfile)
                            div 1000 handle _ => ~1  (* ST.size returns number in bytes *)
            in {cname=head,
                cversion=cversion,
                date=today,
                mach=machine,
                pname=p,
                plen=FileUtil.linesOfFile p,
                ctime=Time.toReal ctime,
                binsz=binsz,
                runs=runs,
                err=""} : line
            end
            handle Fail msg =>
                   (print ("Error: " ^ msg ^ "\n");
                    {cname=head,
                     cversion=cversion,
                     date=today,
                     mach=machine,
                     pname=p,
                     plen=FileUtil.linesOfFile p,
                     ctime= 0.0,
                     binsz= 0,
                     runs=nil,
                     err=msg}) : line
    in map process_prog ps
    end

fun tokenize nil = nil
  | tokenize (s::ss) =
    let  (* `:' should appear as separate items *)
      fun token #":" = true
	| token _ = false
      fun tok (nil, nil, toks) = rev toks
	| tok (nil, acc, toks) = rev (implode(rev acc)::toks)
	| tok (c::cs, acc, toks) =
	  if token c then tok(cs,nil,str c :: implode(rev acc) :: toks)
	  else tok(cs,c::acc,toks)
    in tok (explode s, nil, nil) @ tokenize ss
    end

fun main (progname, args) =
    let
(*
	val _ = print "Args: ["
	val _ = app (fn s => print (s ^ ",")) args
	val _ = print "]\n"
*)
        val args = tokenize args
(*
	val _ = print "Args: ["
	val _ = app (fn s => print (s ^ ",")) args
	val _ = print "]\n"
*)
    in case getCompileArgs (args, nil, NONE) of
           NONE =>
	   (  print "USAGE: mlkit-bench [OPTION]... FILE...\n"
	    ; print "OPTIONS:\n"
	    ; print "  -mlton[:FLAG ... FLAG:]  Run MLTON on each test.\n"
	    ; print "  -mpl[:FLAG ... FLAG:]    Run MPL on each test.\n"
	    ; print "  -mlkit[:FLAG ... FLAG:]  Run MLKIT on each test.\n"
	    ; print "  -o file                  Write json output to `file`.\n"
            ; print "  -r n                     Set number of repetitions to n.\n"
            ; print "  -it                      Use internal timings. Assumes\n"
            ; print "                           usage with Timing.run function in\n"
            ; print "                           par_benchmarks/util/Timing.sml\n"
            ; print "FLAG options to -mlton, -mpl, and -mlkit are passed to\n"
	    ; print "  the compiler, with the exceptions of a flag of the form\n"
            ; print "  K=V, which sets the environment variable K to V during\n"
            ; print "  compilation and during execution.\n"
	    ; print "FILES:\n"
	    ; print "  file.sml         Standard ML source file.\n"
	    ; print "  file.mlb         ML Basis file.\n"
	    ; print "\n"
	    ; print "EXAMPLE:\n"
	    ; print "  The command\n"
            ; print "    $ mlkit-bench -mlkit:-dangle -scratch: -mlton kkb36c.sml\n"
            ; print "  will output benchmark data in json format on stdout. Use the\n"
	    ; print "  -o option to specify an output file. Multiple specifications\n"
            ; print "  of the same compiler is possible.\n"
	    ; print "\n"
	    ; print "ENVIRONMENT:\n"
            ; print "  mlkit-bench makes use of the environment variable MLKIT_ROOT\n"
            ; print "  to locate the MLKit compiler and standard library. For use\n"
            ; print "  with mlton and mpl, mlkit-bench assumes the mlton executables are\n"
            ; print "  installed on the system.\n"
	    ; OS.Process.failure)
	 | SOME (inputs, cs, out) =>
	   let val ps = sourceFiles inputs
	       val ts = List.concat (map (process_progs ps) cs)
               val errs = List.length(List.filter (fn t => #err t <> "") ts)
	       fun withFile NONE f = f TextIO.stdOut
		 | withFile (SOME s) f =
		   let val os = TextIO.openOut s
		   in (  f os
		       ; TextIO.closeOut os
		       ; print ("Wrote file ``" ^ s ^ "''\n"))
		      handle ? => (TextIO.closeOut os; raise ?)
		   end
               val () = if errs = 0 then
                          print "Success: there were no errors.\n"
                        else print ("*** WARNING: there " ^
                                    (if errs = 1 then "was one error.\n"
                                     else "were " ^ Int.toString errs ^ " errors.\n"))
	   in withFile out (fn os => TextIO.output(os, Json.toString (toJson ts)))
	    ; OS.Process.success
	   end
    end
end

structure Main : sig end = struct
val res = Bench.main (CommandLine.name(), CommandLine.arguments())
val _ = OS.Process.exit res
end
