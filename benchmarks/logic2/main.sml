structure Main =
  struct
    exception Done

    fun doit () = Data.exists(fn Z => Data.solution2 Z (fn () => raise Done))
	  handle Done => print "Yes\n"

  end

val _ = Main.doit()
