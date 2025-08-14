(* main.sml *)

structure Main =
  struct
    val name = "Logic"

    exception Done

    fun testit strm = Data.exists(fn Z => Data.solution2 Z (fn () => raise Done))
	  handle Done => TextIO.output(strm, "yes\n")

    fun doit () = Data.exists(fn Z => Data.solution2 Z (fn () => raise Done))
	  handle Done => print "Yes\n"

    fun repeat n f = if n <= 0 then ()
                     else (f(); repeat (n-1) f)

  end; (* Main *)

val _ = Main.repeat 5 Main.doit
