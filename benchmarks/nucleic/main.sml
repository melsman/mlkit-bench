(* main.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *)

structure Main =
  struct
    val name = "Nucleic"
    fun doit () = Nucleic.anticodon_length ()
    fun testit n =
        if n <= 0 then print "Done\n"
        else (if doit() <> 179 then
                print "Error\n"
              else
                (print "Ok - result is 179\n";
                 testit (n-1)))

  end

val _ = Main.testit 40
