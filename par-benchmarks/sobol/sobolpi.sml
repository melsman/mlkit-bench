
structure Sobol = Sobol(val D = 2
                        structure SobolDir = SobolDir50)

structure S = SOAC

val gcs : S.gcs =
    (CommandLineArgs.parseInt "P" 50,
     CommandLineArgs.parseInt "G" 10000)

val N = CommandLineArgs.parseInt "N" 1000000

local
  fun conv x =  (* real(Word64.toLargeInt(Word32.toLarge x)) / Sobol.norm *)
      let val x' = Word32.>>(x,0w2)
          val y = Word32.andb(x,0w3)
      in (real (Word32.toInt y) + 4.0*real(Word32.toInt x'))
         / Sobol.norm
      end

  val endTiming = Timing.start "Computing pi..."
  val vs = S.map (fn i =>
                     let val v = Sobol.independent i
                         val x = conv(Array.sub(v,0))
                         val y = conv(Array.sub(v,1))
                     in if x*x+y*y <= 1.0 then 1
                        else 0
                     end) (S.iota N)
  val r = S.reduce gcs (op +) 0 vs
  val pi = 4.0 * real r / real N
  val () = endTiming()
in
  fun ppr r = Real.fmt (StringCvt.FIX (SOME 12)) r
  val () = print ("PI: " ^ ppr pi ^ "\n")
  val () = print ("Precision: " ^ ppr (Real.abs(Math.pi - pi)) ^ "\n")

  (* Drawing *)
  val K = CommandLineArgs.parseInt "K" 100
  type image = {height:int, width:int, data: RealArray.array}

  val height = CommandLineArgs.parseInt "height" 768
  val width = CommandLineArgs.parseInt "width" 1024
  fun linterp (a,b,f) = (1.0-f)*a + f*b

  type pixel = int * int * int

  fun realToPixel (r:real) : pixel =
      let val i = floor(linterp(255.0,0.0,r))
      in (i,i,i)
      end

  fun image2ppm out ({data, height, width}: image) =
      let fun onPixel ((r,g,b):pixel) =
              TextIO.output(out,
                            Int.toString r ^ " " ^
                            Int.toString g ^ " " ^
                            Int.toString b ^ "\n")
      in TextIO.output(out,
                       "P3\n" ^
                       Int.toString width ^ " " ^ Int.toString height ^ "\n" ^
                       "255\n")
         before RealArray.app (onPixel o realToPixel) data
      end

  fun cross (x,y) =
      [(x-5,y),(x-4,y),(x-3,y),(x-2,y),(x-1,y),(x,y),(x+1,y),(x+2,y),(x+3,y),(x+4,y),(x+5,y),
       (x,y-5),(x,y-4),(x,y-3),(x,y-2),(x,y-1),(x,y+1),(x,y+2),(x,y+3),(x,y+4),(x,y+5)]

  val img =
    let val data = RealArray.tabulate (height*width,fn _ => 0.0)
        val () = S.app gcs (fn i =>
                               let val v = Sobol.independent i
                                   val x = conv(Array.sub(v,0))
                                   val y = conv(Array.sub(v,1))
                                   val x = round(linterp(0.0,real(width-1),x))
                                   val y = round(linterp(0.0,real(height-1),y))
                                   fun upd (x,y) =
                                       if x < 0 orelse x > width - 1 orelse y < 0 orelse y > height - 1 then ()
                                       else RealArray.update(data,
                                                             y*width+x,
                                                             1.0)
                               in List.app upd (cross (x,y))
                               end) (S.iota K)
    in {width  = width,
        height = height,
        data = data }
    end

  val f = CommandLineArgs.parseString "f" ""

  val _ = if f <> "" then
            let val out = TextIO.openOut f
            in print ("Writing image to " ^ f ^ ".\n")
               before image2ppm out img
               before TextIO.closeOut out
            end
          else print ("-f not passed, so not writing image to file.\n")
end
