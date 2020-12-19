structure Timing : sig val start : string -> unit -> unit
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
end
