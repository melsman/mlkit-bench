(* Adapted from the MPL examples: https://github.com/MPLLang/mpl/tree/master/examples *)

structure CLA = CommandLineArgs

fun usage () =
  let
    val msg =
      "usage: seam-carve [-input INPUT.ppm] [-num-seams N]\n"
  in
    TextIO.output (TextIO.stdErr, msg);
    OS.Process.exit OS.Process.failure
  end

val defFile =
    #dir (OS.Path.splitDirFile (CommandLine.name ())) ^ "/broadwaytower.ppm"
val filename = CLA.parseString "input" defFile

val numSeams = CLA.parseInt "num-seams" 100
val _ = print ("num-seams " ^ Int.toString numSeams ^ "\n")

val endTiming = Timing.start "Read image"
val image = PPM.read filename
val () = endTiming()

val w = #width image
val h = #height image

val _ = print ("height " ^ Int.toString h ^ "\n")
val _ = print ("width " ^ Int.toString w ^ "\n")

val _ =
  if numSeams >= 0 andalso numSeams <= w then ()
  else
    Util.die ("cannot remove " ^ Int.toString numSeams
              ^ " seams from image of width "
              ^ Int.toString w ^ "\n")

val () = Timing.run "Seam carving"
                    (fn {endtiming} =>
                        let val res = SCI.makeSeamCarveIndex numSeams image
                            val () = endtiming()
                        in true (* memo: insert validation here... *)
                        end)
