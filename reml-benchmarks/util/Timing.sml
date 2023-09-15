structure Timing : sig val start : string -> unit -> unit

                       val run : string -> ({endtiming:unit->unit} -> bool) -> unit   (* report timing on stderr; print chk report on stdout *)
                   end =
struct
fun start s : unit -> unit =
    if CommandLineArgs.parseFlag "t" then
      let val t0 = Time.now()
          val () = print ("[" ^ s ^ ": Timing started...]\n")
      in fn () =>
            let val t = Time.now()
            in print ("[" ^ s ^ ": Finished in " ^ Time.fmt 4 (Time.-(t,t0)) ^ "s]\n")
            end
      end
    else fn () => ()

local
  fun print_stderr s =
      (TextIO.output(TextIO.stdErr,s); TextIO.flushOut TextIO.stdErr)
  fun exec f i =
      let val t0 = Time.now()
          val () = print_stderr ("[Run " ^ Int.toString i ^ ": ")
      in f {endtiming=fn()=>print_stderr ( Time.fmt 4 (Time.-(Time.now(),t0)) ^ "s]\n")}
      end
  fun loop f i r it acc =   (* it: internal timings *)
      if i > r then acc
      else let val res = if it
                         then exec f i
                         else f {endtiming=fn () => ()}
           in loop f (i+1) r it (acc andalso res)
           end
in
fun run s (f : {endtiming:unit->unit} -> bool) : unit =
    let val r = CommandLineArgs.parseInt "r" 1
        val it = CommandLineArgs.parseFlag "it"
        val () = print_stderr ("[Runs: " ^ Int.toString r ^ "]\n")
        val () = print ("Benchmark: " ^ s ^ "\n")
        val res = loop f 1 r it true
    in if res then print "Evaluation: OK\n"
       else print "Evaluation: ERR\n"
    end
end
end
