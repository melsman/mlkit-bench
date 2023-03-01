
(* -------------------------------
 * 3D Vectors
 * ------------------------------- *)

local

structure V : sig
  type t
  val mk : {x: real, y: real, z: real} -> t
  val x : t -> real
  val y : t -> real
  val z : t -> real
end = struct
  structure B = Real64Block3
  type t = B.t
  fun mk {x:real,y:real,z:real} = B.pack(x,y,z)
  val x = B.sub0
  val y = B.sub1
  val z = B.sub2
end

type vec3 = V.t

local
    fun vf f (v1: vec3) (v2: vec3) =
        V.mk{x= f (V.x v1, V.x v2),
             y= f (V.y v1, V.y v2),
             z= f (V.z v1, V.z v2)}
in

val vec_add = vf (op+)
val vec_sub = vf (op-)
val vec_mul = vf (op* )
val vec_div = vf (op/)

fun scale s v =
    V.mk {x=s * V.x v, y=s * V.y v, z=s * V.z v}

fun dot (v1: vec3) (v2: vec3) =
    let val v3 = vec_mul v1 v2
    in V.x v3 + V.y v3 + V.z v3 end

fun norm v = Math.sqrt (dot v v)

fun normalise v = scale (1.0 / norm v) v

fun cross (v1, v2) =
    V.mk{x=V.y v1 * V.z v2 - V.z v1 * V.y v2,
         y=V.z v1 * V.x v2 - V.x v1 * V.z v2,
         z=V.x v1 * V.y v2 - V.y v1 * V.x v2}

val zero : vec3 = V.mk{x=0.0,y=0.0,z=0.0}

end

(* -------------------------------
 * Colors and images
 * ------------------------------- *)

type color = vec3

type pixel = int * int * int

fun colorToPixel (c:color) : pixel =
    let val ir = trunc (255.99 * V.x c)
        val ig = trunc (255.99 * V.y c)
        val ib = trunc (255.99 * V.z c)
    in (ir, ig, ib)
    end

type image = {height:int, width:int, data: RealArray.array}

val height = CommandLineArgs.parseInt "height" 768
val width = CommandLineArgs.parseInt "width" 1024
val M = CommandLineArgs.parseInt "M" 10000  (* max iteration *)

fun pow2 0 = 1
  | pow2 n = 2*pow2(n-1)

fun linterp (a,b,f) = (1.0-f)*a + f*b
fun linterp_vec3 (v1,v2,f) = vec_add (scale (1.0-f) v1) (scale f v2)

val white = V.mk{x=1.0, y=1.0, z=1.0}
val black = scale 0.0 white
val grey = scale 0.5 white
val red = V.mk{x=1.0, y=0.0, z=0.0}
val green = V.mk{x=0.0, y=1.0, z=0.0}
val blue = V.mk{x=0.0, y=0.0, z=1.0}

fun relax c = vec_add (scale 0.8 c) (scale 0.1 grey)

val C = 10

fun palette (M:int) (i:int) : vec3 =  (* i > M for using the entire palette *)
    let val f = real (i mod M) / real M
        val c4 = linterp_vec3(red,green,f)
        val c5 = linterp_vec3(green,blue,f)
    in relax(linterp_vec3(c4,c5,f))
    end

fun realToColor (r:real) : color =
    let val color1 = palette C (floor r)
        val color2 = palette C (floor r + 1)
        val fraction = r - real(floor r)
        val color = linterp_vec3(color1, color2, fraction)
    in color
    end


val bailout : real = real(pow2 8)

val d0 = ((~2.5,1.0),(~1.0,1.0))
val d1 = ((~0.95,~0.85),(0.25,0.35))
val d2 = ((~0.95,~0.9),(0.25,0.3))

val ds = [d0,d1,d2]

fun get ds x = List.nth(ds,x) handle _ => d0

val d = CommandLineArgs.parseInt "d" 0

val x1 = CommandLineArgs.parseReal "x1" (#1(#1(get ds d)))
val x2 = CommandLineArgs.parseReal "x2" (#2(#1(get ds d)))
val y1 = CommandLineArgs.parseReal "y1" (#1(#2(get ds d)))
val y2 = CommandLineArgs.parseReal "y2" (#2(#2(get ds d)))

fun mandel (Py, Px) : real =
    let val x0 = linterp(x1, x2, real Px / real width)
        val y0 = linterp(y1, y2, real Py / real height)
        (* Here N = 2^8 is chosen as a reasonable bailout radius. *)
        val innerM = 100
        fun innerloop (i, x, y) =
            if x*x + y*y <= bailout andalso i < innerM
            then innerloop (i+1, x*x - y*y + x0, 2.0*x*y + y0)
            else (i, x+0.0, y+0.0)
        fun loop (i, x, y) =
            let val (j,x,y) = innerloop (0,x,y)
            in if x*x + y*y <= bailout andalso (i+j) < M
               then loop (i+j,x,y)
               else (i+j, x+0.0, y+0.0)
            end
        val (i, x, y) = loop (0, 0.0, 0.0)
    in
      (* Avoid floating point issues with points inside the set. *)
      if i < M then
        (* sqrt of inner term removed using log simplification rules *)
        let val log_zn = Math.ln(x*x + y*y) / 2.0
            val nu = Math.ln(log_zn / Math.ln 2.0) / Math.ln 2.0
        (* Rearranging the potential function.
         * Dividing log_zn by log(2) instead of log(N = 1<<8)
         * because we want the entire palette to range from the
         * center to radius 2, NOT our bailout radius. *)
        in real(i+1)-nu
        end
      else real i
    end

fun image2ppm out ({data, height, width}: image) =
    let fun onPixel (r,g,b) =
            TextIO.output(out,
                          Int.toString r ^ " " ^
                          Int.toString g ^ " " ^
                          Int.toString b ^ "\n")
    in TextIO.output(out,
                     "P3\n" ^
                     Int.toString width ^ " " ^ Int.toString height ^ "\n" ^
                     "255\n")
       before RealArray.app (onPixel o colorToPixel o realToColor) data
    end

fun image2ppm6 out ({data, height, width}: image) =
    let fun onPixel (r,g,b) =
            TextIO.output(out, String.implode (List.map Char.chr [r,g,b]))
    in TextIO.output(out,
                     "P6\n" ^
                     Int.toString width ^ " " ^ Int.toString height ^ "\n" ^
                     "255\n")
       before RealArray.app (onPixel o colorToPixel o realToColor) data
    end

val gcs : ForkJoin.gcs =
    (CommandLineArgs.parseInt "P" 50,
     CommandLineArgs.parseInt "G" 10)

fun for (lo,hi) (f:int->unit) : unit =
    let fun loop i =
            if i >= hi then ()
            else (f i; loop (i+1))
    in loop lo
    end

fun mk_img () =
    let val data = RealArray.tabulate (height*width,fn _ => 0.0)
        val () = ForkJoin.parfor' gcs (0,height-1)
                                  (fn y => for (0,width)
                                               (fn x => RealArray.update(data,
                                                                         y*width+x,
                                                                         mandel(y,x))))
    in {width  = width,
        height = height,
        data = data }
    end



val f = CommandLineArgs.parseString "f" ""
val dop6 = CommandLineArgs.parseFlag "ppm6"

val writeImage = if dop6 then image2ppm6 else image2ppm
in
val _ = if f <> "" then
          let val out = TextIO.openOut f
              val endTiming = Timing.start "Start image construction"
              val img = mk_img()
              val () = endTiming()
          in print ("Writing image to " ^ f ^ ".\n")
             before writeImage out img
             before TextIO.closeOut out
          end
        else ( print ("-f not passed, so not writing image to file.\n")
             ; Timing.run "Generating Mandelbrot image"
                          (fn {endtiming} =>
                              let val img = mk_img()
                                  val () = endtiming()
                              in  #height img = height
                              end) )
end
