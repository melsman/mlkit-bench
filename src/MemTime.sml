signature MEM_TIME =
  sig
    type measurement
    val memTime : {cmd: string, args: string list,
                   out_file: string, eout_file: string option} -> measurement
    val memTime' : {cmd: string, args: string list,
                    out_file: string, eout_file: string option} -> measurement list
    val pp      : measurement -> string
  end

(**

[memTime {cmd, args, out_file}] executes cmd with args and reports its
memory and time usage. Output from the command is redirected to
out_file.  It is assumed that cmd does not write on stderr.

[memTime' {cmd, args, out_file}] executes cmd with args and reports
its memory and time usage by writing to stderr (1) a line of the form
"[Runs: N]" followed by N lines of the form "[Run N: X.XXXs]" where
X.XXX is the real time in seconds and N is the run number (1 to N).
Output from the command is redirected to out_file.

*)


structure SysUtil = struct

fun quot s = "'" ^ s ^ "'"

fun error s = (print s; raise Fail s)

fun vchat0 (verbose:unit -> bool) s =
    if verbose() then print (" ++ " ^ s ^ "\n")
    else ()

fun system_v verbose cmd : unit =
    (vchat0 verbose ("Executing command: " ^ cmd);
     let val status = OS.Process.system cmd
		      handle _ => error ("Command failed: " ^ quot cmd)
     in if OS.Process.isSuccess status then ()
        else error ("Command failed: " ^ quot cmd)
     end)

fun system cmd : unit = system_v (fn () => false) cmd
end


structure MemTime : MEM_TIME where type measurement = DataType.measurement =
  struct

    type measurement = DataType.measurement

    fun pp {rss,size,data,stk,exe,sys,user,real,gc,gcn,majgc,majgcn} =
        let val pr_r = Time.toString o Time.fromReal
        in "rss: " ^ (Int.toString rss) ^
           "Kb.\nsize: " ^ (Int.toString size) ^ "Kb.\ndata: " ^ (Int.toString data) ^
           "Kb.\nstk: " ^ (Int.toString stk) ^ "Kb.\nexe: " ^ (Int.toString exe) ^
           "Kb.\nsys: " ^ pr_r sys ^ "sec.\nuser: " ^ pr_r user ^
           "sec.\nreal: " ^ pr_r real ^
           "sec.\ngc: " ^ pr_r gc ^
           "sec.\ngcn: " ^ Int.toString gcn ^
           ".\nmajgc: " ^ pr_r majgc ^
           "sec.\nmajgcn: " ^ Int.toString majgcn ^ ".\n"
        end

    fun readAll f =
        let val is = TextIO.openIn f
            val s = TextIO.inputAll is handle X => (TextIO.closeIn is; raise X)
            val () = TextIO.closeIn is
        in s
        end

    fun splitLines s = String.tokens (fn c => c = #"\n") s

    fun gcinfo (lines:string list) : {gc:real,majgc:real,gcn:int,majgcn:int} option =
        case List.find (String.isPrefix "[GC(") lines of
            NONE => NONE
          | SOME l =>
            let val l = String.extract(l,4,NONE)
                fun readTime s = if String.isSuffix "ms)" s then
                                   Option.map (fn r => r / 1000.0) (Real.fromString s)
                                 else NONE
                fun process gc gcn majgc majgcn =
                    case (readTime gc, Int.fromString gcn, readTime majgc, Int.fromString majgcn) of
                        (SOME gct,SOME gcn, SOME majgct, SOME majgcn) => SOME {gc=gct,majgc=majgct,
                                                                           gcn=gcn,majgcn=majgcn}
                      | _ => NONE
            in case String.tokens (fn c => c = #" " orelse c = #":"
                                           orelse c = #"(" orelse c = #",") l of
                   gc :: gcn :: "collections" :: majgcn :: "major" :: majgc :: _ =>
                   process gc gcn majgc majgcn
                |  gc :: gcn :: "collections" :: _ =>
                   process gc gcn gc gcn
                | _ => NONE
            end

(* macos format of '/usr/bin/time -lp':
real         0.22
user         0.18
sys          0.02
  18812928  maximum resident set size
         0  average shared memory size
         0  average unshared data size
         0  average unshared stack size
     13338  page reclaims
         0  page faults
         0  swaps
         0  block input operations
         0  block output operations
         0  messages sent
         0  messages received
         0  signals received
         0  voluntary context switches
        62  involuntary context switches
*)

    local
      fun lookR lines s =
          case List.find (String.isSuffix s) lines of
              SOME l => (case Real.fromString l of
                             SOME v => v
                           | NONE => raise Fail ("lookR: cannot read real in line '" ^ l ^ "'"))
            | NONE => raise Fail ("lookR: cannot find line with string '" ^ s ^ "' in output:" ^
                                 String.concatWith "\n" lines)
      fun lookL lines s =
          case List.find (String.isPrefix s) lines of
              SOME l =>
              let val k = String.extract(l,size s,NONE)
              in (case Real.fromString k of
                      SOME v => v
                    | NONE => raise Fail ("lookL: cannot read real in line '" ^ l ^ "'"))
              end
            | NONE => raise Fail ("lookL: cannot find line with string '" ^ s ^ "'")
    in
      fun memTime_macos {cmd, args, out_file, eout_file} : measurement =
          let val timeout = case eout_file of
                                SOME f => f
                              | NONE => "time.out"
              val args = String.concatWith " " args
              val command = cmd ^ " " ^ args ^ " > " ^ out_file
              val command2 = "/usr/bin/env time -lp " ^ command ^ " 2> " ^ timeout
              val () = OS.FileSys.remove timeout handle _ => ()
              val () = SysUtil.system command2 handle _ => SysUtil.error "Failed to execute command"
              val s = readAll timeout handle _ => SysUtil.error "Failed to read output file"
              val lines = splitLines s
              val rss = Real.floor (lookR lines "maximum resident set size")
              val rss = rss div 1000
              val size = 0
              val data = 0
              val stk = 0
              val exe = 0
              val sys = lookL lines "sys"
              val real = lookL lines "real"
              val user = lookL lines "user"
              val {gc,gcn,majgc,majgcn} =
                  case gcinfo lines of
                      SOME r => r
                    | MONE => {gc=0.0,gcn=0,majgc=0.0,majgcn=0}
          in {size=size,rss=rss,
	      data=data,stk=stk,exe=exe,
	      sys=sys,
	      user=user,
	      real=real,
              gc=gc,gcn=gcn,majgc=majgc,majgcn=majgcn}
          end
      fun getRss_macos lines =
          Real.floor (lookR lines "maximum resident set size") div 1000
      val cmd_prefix_macos = "/usr/bin/env time -lp "
    end

(* linux format of '/usr/bin/time -v':
	Command being timed: "./lexgen"
	User time (seconds): 0.74
	System time (seconds): 0.04
	Percent of CPU this job got: 96%
	Elapsed (wall clock) time (h:mm:ss or m:ss): 0:00.81
	Average shared text size (kbytes): 0
	Average unshared data size (kbytes): 0
	Average stack size (kbytes): 0
	Average total size (kbytes): 0
	Maximum resident set size (kbytes): 18808
	Average resident set size (kbytes): 0
	Major (requiring I/O) page faults: 0
	Minor (reclaiming a frame) page faults: 19005
	Voluntary context switches: 39
	Involuntary context switches: 1
	Swaps: 0
	File system inputs: 0
	File system outputs: 8960
	Socket messages sent: 0
	Socket messages received: 0
	Signals delivered: 0
	Page size (bytes): 4096
	Exit status: 0
*)

    local
      fun findLine lines s : string =
          case List.find (String.isSubstring s) lines of
              SOME l => l
            | NONE => raise Fail ("findLine: cannot find line with string '" ^ s ^ "' in output" ^
                                  String.concatWith "\n" lines)

      fun look lines s : real =
          let val l = findLine lines s
          in case String.tokens (fn c => c = #":") l of
                 [_,v] => (case Real.fromString v of
                               SOME r => r
                             | NONE => raise Fail ("look: cannot read real in line '" ^ l ^ "'"))
               | _ => raise Fail ("look: failed to find a single ':' in '" ^ l ^ "'")
          end

      fun maybeElimZeroPrefix s =
          if s = "0" then s
          else if size s > 0 then
            if String.sub(s,0) = #" " then maybeElimZeroPrefix(String.extract(s,1,NONE))
            else if String.sub(s,0) = #"0" then String.extract(s,1,NONE)
            else s
          else s

      fun lookTime lines s : real =
          let val l = findLine lines s
          in case rev(String.tokens (fn c => c = #":") l) of
                 s :: m :: _ =>
                 (case Int.fromString (maybeElimZeroPrefix m) of
                      SOME m =>
                      (case Real.fromString s of
                           SOME s => s + real (60*m)
                         | NONE => raise Fail ("lookTime: failed to find seconds in '" ^ l ^ "'"))
                    | NONE => raise Fail ("lookTime: failed to find minutes in '" ^ l ^ "' (m='" ^ m ^ "', m2='" ^ maybeElimZeroPrefix m ^ "')"))
               | _ => raise Fail ("lookTime: failed to find ':' in '" ^ l ^ "'")
          end
    in
      fun memTime_linux {cmd, args, out_file, eout_file} : measurement =
        let val timeout = case eout_file of
                              SOME f => f
                            | NONE => "time.out"
            val args = String.concatWith " " args
            val command = cmd ^ " " ^ args ^ " > " ^ out_file
            val command2 = "/usr/bin/env time -v " ^ command ^ " 2> " ^ timeout
            val () = OS.FileSys.remove timeout handle _ => ()
            val () = SysUtil.system command2
                     handle _ => SysUtil.error ("Failed to execute command '" ^ command2 ^ "'")
            val s = readAll timeout
                    handle _ => SysUtil.error ("Failed to read output file " ^ timeout)
            val lines = splitLines s
            val rss = Real.floor (look lines "Maximum resident set size")
            val size = 0
            val data = 0
            val stk = 0
            val exe = 0
            val sys = look lines "System time"
            val real = lookTime lines "Elapsed (wall clock) time"
            val user = look lines "User time"
            val {gc,gcn,majgc,majgcn} =
                case gcinfo lines of
                    SOME r => r
                  | MONE => {gc=0.0,gcn=0,majgc=0.0,majgcn=0}
        in {size=size,rss=rss,
	    data=data,stk=stk,exe=exe,
	    sys=sys,
	    user=user,
	    real=real,
            gc=gc,gcn=gcn,majgc=majgc,majgcn=majgcn}
        end
      fun getRss_linux lines = Real.floor (look lines "Maximum resident set size")
      val cmd_prefix_linux = "/usr/bin/env time -v "
    end

    fun sysname () =
        let fun look s l =
                case l of
                    nil => NONE
                  | (k,v)::l => if s = k then SOME v else look s l
        in look "sysname" (Posix.ProcEnv.uname())
        end

    fun memTime x =
        case sysname() of
            SOME "Darwin" => memTime_macos x
          | _ => memTime_linux x


    fun memTime0' cmd_prefix getRss {cmd, args, out_file, eout_file} : measurement list =
        let val timeout = case eout_file of
                              SOME f => f
                            | NONE => "time.out"
            val args = String.concatWith " " args
            val command = cmd ^ " " ^ args ^ " > " ^ out_file
            val command2 = cmd_prefix ^ command ^ " 2> " ^ timeout
            val () = OS.FileSys.remove timeout handle _ => ()
            val () = SysUtil.system command2
                     handle _ => SysUtil.error ("Failed to execute command '" ^ command2 ^ "'")
            val s = readAll timeout
                    handle _ => SysUtil.error ("Failed to read output file " ^ timeout)
            val lines = splitLines s
            val rss = getRss lines
            val runs = List.filter (fn l => String.isPrefix "[Run " l) lines
        in List.map (fn r =>
                        let val realt =
                                case String.tokens (fn c => c = #":") r of
                                    [_,s] => (case Real.fromString s of
                                                  SOME r => r
                                                | NONE => 0.0)
                                  | _ => 0.0
                        in {real=realt, rss=rss, size=0, data=0, stk=0, exe=0, sys=0.0,
	                    user=0.0, gc=0.0, gcn=0, majgc=0.0, majgcn=0}
                        end) runs
        end

    fun memTime' x =
        case sysname() of
            SOME "Darwin" => memTime0' cmd_prefix_macos getRss_macos x
          | _ => memTime0' cmd_prefix_linux getRss_linux x


  end
