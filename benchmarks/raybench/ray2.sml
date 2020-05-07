structure ForkJoin = struct

fun par (f,g) = (f(),g())
fun alloc n v = Array.tabulate(n,fn _ => v)
fun parfor (f:int->unit) X (lo, hi) : unit =
    if lo >= hi then () else (f lo; parfor f X (lo+1, hi))

end

(* A ray tracer that fires one ray per pixel and only supports
coloured, reflective spheres.  It parallelises two things

 0. The construction of a BVH for accelerating ray lookups
    (divide-and-conquer task parallelism)

 1. The parallel loop across all of the pixels to be computed (data
    parallelism, albeit potentially poorly load balanced)

*)

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

type aabb = { min: vec3, max: vec3 }

local
  structure B = Real64Block6
in
type aabb_packed = B.t

fun aabb_pack { min: vec3, max: vec3 } : aabb_packed =
    B.pack(V.x min, V.y min, V.z min,
           V.x max, V.y max, V.z max)

fun aabb_unpack (t : aabb_packed) : aabb =
    {min=V.mk{x=B.sub0 t,y=B.sub1 t,z=B.sub2 t},
     max=V.mk{x=B.sub3 t,y=B.sub4 t,z=B.sub5 t}}
end

fun min x y : real = Real.min(x,y)
fun max x y : real = Real.max(x,y)

fun enclosing (box0: aabb_packed) (box1: aabb_packed) =
    let val box0 = aabb_unpack box0
        val box1 = aabb_unpack box1
        val small = V.mk{ x = min (V.x (#min box0)) (V.x (#min box1))
                        , y = min (V.y (#min box0)) (V.y (#min box1))
                        , z = min (V.z (#min box0)) (V.z (#min box1))
                        }
        val big = V.mk{ x = max (V.x (#max box0)) (V.x (#max box1))
                      , y = max (V.y (#max box0)) (V.y (#max box1))
                      , z = max (V.z (#max box0)) (V.z (#max box1))
                  }
  in aabb_pack {min=small, max=big} end

fun centre (aabb: aabb_packed) =
    let val aabb = aabb_unpack aabb
    in V.mk{ x = (V.x (#min aabb) + (V.x (#max aabb) - V.x (#min aabb))),
             y = (V.y (#min aabb) + (V.y (#max aabb) - V.y (#min aabb))),
             z = (V.z (#min aabb) + (V.z (#max aabb) - V.z (#min aabb)))
       }
    end

datatype 'a bvh = bvh_leaf of aabb_packed * 'a
                | bvh_split of aabb_packed * 'a bvh * 'a bvh

fun bvh_aabb (bvh_leaf (box, _)) = box
  | bvh_aabb (bvh_split (box, _, _)) = box

(* Couldn't find a sorting function in MLtons stdlib - this is from Rosetta Code. *)
local
    fun merge cmp ([], ys) = ys
      | merge cmp (xs, []) = xs
      | merge cmp (xs as x::xs', ys as y::ys') =
          case cmp (x, y) of
               GREATER => y :: merge cmp (xs, ys')
             | _       => x :: merge cmp (xs', ys)
    fun sort cmp [] = []
      | sort cmp [x] = [x]
      | sort cmp xs =
        let
          val ys = List.take (xs, length xs div 2)
          val zs = List.drop (xs, length xs div 2)
        in
          merge cmp (sort cmp ys, sort cmp zs)
        end
in
fun mk_bvh f all_objs =
    let fun mk _ _ [] = raise Fail "mk_bvh: no nodes"
          | mk _ _ [x] = bvh_leaf(f x, x)
          | mk d n xs =
            let val axis = case d mod 3 of 0 => V.x
                                         | 1 => V.y
                                         | _ => V.z
                fun cmp (x, y) =
                    Real.compare(axis(centre(f x)),
                                 axis(centre(f y)))
                val xs_sorted = sort cmp xs
                val xs_left = List.take(xs_sorted, n div 2)
                val xs_right = List.drop(xs_sorted, n div 2)
                fun do_left () = mk (d+1) (n div 2) xs_left
                fun do_right () = mk (d+1) (n-(n div 2)) xs_right
                val (left, right) =
                    if n < 100
                    then (do_left(), do_right())
                    else ForkJoin.par (do_left, do_right)
                val box = enclosing (bvh_aabb left) (bvh_aabb right)
            in bvh_split (box, left, right) end
    in mk 0 (length all_objs) all_objs end
end

type pos = vec3
type dir = vec3
type colour = vec3

val black : vec3 = V.mk{x=0.0, y=0.0, z=0.0}
val white : vec3 = V.mk{x=1.0, y=1.0, z=1.0}

type ray = {origin: pos, dir: dir}

fun point_at_param (ray: ray) t =
    vec_add (#origin ray) (scale t (#dir ray))

type hit = { t: real
           , p: pos
           , normal: dir
           , colour: colour
           }

local
  structure B = Real64Block10
in
type hit_packed = B.t

fun hit_pack ({t,p,normal=n,colour=c}:hit) : hit_packed =
    B.pack(t,V.x p,V.y p,V.z p,V.x n, V.y n, V.z n,V.x c, V.y c, V.z c)

fun hit_unpack (t : hit_packed) : hit =
    {t=B.sub0 t,p=V.mk{x=B.sub1 t,y=B.sub2 t,z=B.sub3 t},
     normal=V.mk{x=B.sub4 t,y=B.sub5 t,z=B.sub6 t},
     colour=V.mk{x=B.sub7 t,y=B.sub8 t,z=B.sub9 t}}

fun hit_t (t: hit_packed) = B.sub0 t
end

type sphere = { pos: pos
              , colour: colour
              , radius: real
              }
local
  structure B = Real64Block7
in
type sphere_packed = B.t
fun sphere_pack ({pos=p,colour=c,radius=r}:sphere) : sphere_packed =
    B.pack(V.x p,V.y p,V.z p,V.x c,V.y c,V.z c,r)
fun sphere_unpack (t : sphere_packed) : sphere =
    {pos=V.mk{x=B.sub0 t,y=B.sub1 t,z=B.sub2 t},
     colour=V.mk{x=B.sub3 t,y=B.sub4 t,z=B.sub5 t},
     radius=B.sub6 t}
end

fun sphere_aabb (sphere:sphere_packed) =
    let val {pos, colour=_, radius} = sphere_unpack sphere
    in aabb_pack {min = vec_sub pos (V.mk{x=radius, y=radius, z=radius}),
                  max = vec_add pos (V.mk{x=radius, y=radius, z=radius})}
    end

fun sphere_hit (sphere:sphere_packed) (r:ray) t_min t_max : hit_packed option =
    let val {pos, colour, radius} = sphere_unpack sphere
        fun try temp =
            if temp < t_max andalso temp > t_min
            then SOME let val p = point_at_param r temp
                          val hit = { t = temp+0.0
                                    , p = p
                                    , normal = scale (1.0/radius)
                                                     (vec_sub p pos)
                                    , colour = colour
                                    }
                      in hit_pack hit
                      end
            else NONE
        val oc = vec_sub (#origin r) pos
        val dr = #dir r
        val a = dot dr dr
        val b = dot oc dr
        val c = dot oc oc - radius*radius
        val discriminant = b*b - a*c
    in if discriminant <= 0.0
       then NONE
       else let val t = Math.sqrt discriminant
            in case try ((~b - t)/a) of
                   SOME hit => SOME hit
                 | NONE => try ((~b + t)/a)
            end
    end

fun aabb_hit (aabb:aabb_packed) ({origin, dir}: ray) tmin0 tmax0 =
  let fun iter min' max' origin' dir' tmin' tmax' =
          let val invD = 1.0 / dir'
              val t0 = (min' - origin') * invD
              val t1 = (max' - origin') * invD
              val t0' = if invD < 0.0 then t1 else t0
              val t1' = if invD < 0.0 then t0 else t1
(*              val (t0', t1') = if invD < 0.0 then (t1, t0) else (t0, t1) *)
              val tmin'' = max t0' tmin'
              val tmax'' = min t1' tmax'
          in (tmin'', tmax'') end
      val aabb = aabb_unpack aabb
      val (tmin1, tmax1) =
          iter
          (V.x (#min aabb)) (V.x (#max aabb))
          (V.x origin) (V.x dir)
          tmin0 tmax0
  in if tmax1 <= tmin1 then false
     else let val (tmin2, tmax2) =
                  iter (V.y (#min aabb)) (V.y (#max aabb))
                  (V.y origin) (V.y dir)
                  tmin1 tmax1
          in if tmax2 <= tmin2 then false
             else let val (tmin3, tmax3) =
                          iter (V.z (#min aabb)) (V.z (#max aabb))
                          (V.z origin) (V.z dir)
                          tmin2 tmax2
                  in not (tmax3 <= tmin3) end
          end
  end

type objs = sphere_packed bvh

fun objs_hit (bvh_leaf (v, s)) r t_min t_max : hit_packed option =
    sphere_hit s r t_min t_max
  | objs_hit (bvh_split (box, left, right)) r t_min t_max =
    if not (aabb_hit box r t_min t_max)
    then NONE
    else case objs_hit left r t_min t_max of
             SOME h => (case objs_hit right r t_min (hit_t h) of
                            NONE => SOME h
                          | SOME h' => SOME h')
           | NONE => objs_hit right r t_min t_max

type camera = { origin: pos
              , llc: pos
              , horizontal: dir
              , vertical: dir
              }

fun camera lookfrom lookat vup vfov aspect =
  let val theta = vfov * Math.pi / 180.0
      val half_height = Math.tan (theta / 2.0)
      val half_width = aspect * half_height
      val origin = lookfrom
      val w = normalise (vec_sub lookfrom lookat)
      val u = normalise (cross (vup, w))
      val v = cross (w, u)
  in { origin = lookfrom
     , llc = vec_sub
             (vec_sub (vec_sub origin (scale half_width u))
                     (scale half_height v)) w
     , horizontal = scale (2.0*half_width) u
     , vertical = scale (2.0*half_height) v
     }
  end

fun get_ray (cam: camera) s t : ray =
    { origin = #origin cam
    , dir = vec_sub (vec_add (vec_add (#llc cam) (scale s (#horizontal cam)))
                             (scale t (#vertical cam)))
                    (#origin cam)
    }

fun reflect v n =
    vec_sub v (scale (2.0 * dot v n) n)

fun scatter (r: ray) (hit: hit_packed) =
    let val hit = hit_unpack hit
        val reflected =
            reflect (normalise (#dir r)) (#normal hit)
        val scattered = {origin = #p hit, dir = reflected}
    in if dot (#dir scattered) (#normal hit) > 0.0
       then SOME (scattered, #colour hit)
       else NONE
    end

fun ray_colour objs r depth =
    case objs_hit objs r 0.001 1000000000.0 of
        SOME hit => (case scatter r hit of
                         SOME (scattered, attenuation) =>
                         if depth < 50
                         then vec_mul attenuation (ray_colour objs scattered (depth+1))
                         else black
                       | NONE => black)
      | NONE => let val unit_dir = normalise (#dir r)
                    val t = 0.5 * (V.y unit_dir + 1.0)
                    val bg = V.mk{x=0.5, y=0.7, z=1.0}
                in vec_add (scale (1.0-t) white) (scale t bg)
                end

fun trace_ray objs width height cam j i : colour =
    let val u = real i / real width
        val v = real j / real height
        val ray = get_ray cam u v
    in ray_colour objs ray 0 end

type pixel = int * int * int

fun colour_to_pixel (c:colour) : pixel =
    let val ir = trunc (255.99 * V.x c)
        val ig = trunc (255.99 * V.y c)
        val ib = trunc (255.99 * V.z c)
    in (ir, ig, ib) end

type image = { pixels: pixel Array.array
             , height: int
             , width: int}

fun image2ppm out ({pixels, height, width}: image) =
    let fun onPixel (r,g,b) =
            TextIO.output(out,
                          Int.toString r ^ " " ^
                          Int.toString g ^ " " ^
                          Int.toString b ^ "\n")
    in TextIO.output(out,
                     "P3\n" ^
                     Int.toString width ^ " " ^ Int.toString height ^ "\n" ^
                     "255\n")
       before Array.app onPixel pixels
    end

fun image2ppm6 out ({pixels, height, width}: image) =
    let
      fun onPixel (r,g,b) =
        TextIO.output(out, String.implode (List.map Char.chr [r,g,b]))
    in TextIO.output(out,
                     "P6\n" ^
                     Int.toString width ^ " " ^ Int.toString height ^ "\n" ^
                     "255\n")
       before Array.app onPixel pixels
    end

fun render objs width height cam : image =
    let val pixels = ForkJoin.alloc (height*width) (0,0,0)
        fun pixel l =
            let val i = l mod width
                val j = height - l div width
            in Array.update (pixels,
                             l,
                             colour_to_pixel (trace_ray objs width height cam j i)) end
        val _ = ForkJoin.parfor pixel 256 (0,height*width)
    in {width = width,
        height = height,
        pixels = pixels
       }
    end

type scene = { camLookFrom: pos
             , camLookAt: pos
             , camFov: real
             , spheres: sphere_packed list
             }

fun from_scene width height (scene: scene) : objs * camera =
  (mk_bvh sphere_aabb (#spheres scene),
   camera (#camLookFrom scene) (#camLookAt scene) (V.mk{x=0.0, y=1.0, z=0.0})
   (#camFov scene) (real width/real height))

fun tabulate_2d m n f =
    List.concat (List.tabulate (m, fn j => List.tabulate (n, fn i => f (j, i))))

val rgbbox : scene =
    let val n = 10
        val k = 60.0
        val leftwall =
            tabulate_2d n n (fn (y, z) =>
                                sphere_pack{ pos=V.mk{x=(~k/2.0),
                                                      y=(~k/2.0 + (k/real n) * real y),
                                                      z=(~k/2.0 + (k/real n) * real z)}
                                           , colour=V.mk{x=1.0, y=0.0, z=0.0}
                                           , radius = (k/(real n*2.0))
                                           })
        val midwall =
            tabulate_2d n n (fn (x,y) =>
                                sphere_pack{ pos=V.mk{x=(~k/2.0 + (k/real n) * real x),
                                                      y=(~k/2.0 + (k/real n) * real y),
                                                      z=(~k/2.0)}
                                           , colour=V.mk{x=1.0, y=1.0, z=0.0}
                                           , radius = (k/(real n*2.0))})
        val rightwall =
            tabulate_2d n n (fn (y,z) =>
                                sphere_pack{ pos=V.mk{x=(k/2.0),
                                                      y=(~k/2.0 + (k/real n) * real y),
                                                      z=(~k/2.0 + (k/real n) * real z)}
                                           , colour=V.mk{x=0.0, y=0.0, z=1.0}
                                           , radius = (k/(real n*2.0))
                                           })
        val bottom =
            tabulate_2d n n (fn (x,z) =>
                                sphere_pack{ pos=V.mk{x=(~k/2.0 + (k/real n) * real x),
                                                      y=(~k/2.0),
                                                      z=(~k/2.0 + (k/real n) * real z)}
                                           , colour=V.mk{x=1.0, y=1.0, z=1.0}
                                           , radius = (k/(real n*2.0))
                                           })
    in { spheres = leftwall @ midwall @ rightwall @ bottom
       , camLookFrom = V.mk{x=0.0, y=30.0, z=30.0}
       , camLookAt = V.mk{x=0.0, y= ~1.0, z= ~1.0}
       , camFov = 75.0
       }
    end

val irreg : scene =
    let val n = 100
        val k = 600.0
        val bottom =
            tabulate_2d n n (fn (x,z) =>
                                sphere_pack{ pos=V.mk{x=(~k/2.0 + (k/real n) * real x),
                                                      y=0.0,
                                                      z=(~k/2.0 + (k/real n) * real z)}
                                           , colour = white
                                           , radius = k/(real n * 2.0)
                                           })
    in { spheres = bottom
       , camLookFrom = V.mk{x=0.0, y=12.0, z=30.0}
       , camLookAt = V.mk{x=0.0, y=10.0, z= ~1.0}
       , camFov = 75.0 }
    end

val height = CommandLineArgs.parseInt "m" 200
val width = CommandLineArgs.parseInt "n" 200
val f = CommandLineArgs.parseString "f" ""
val dop6 = CommandLineArgs.parseFlag "ppm6"
val scene_name = CommandLineArgs.parseString "s" "rgbbox"
val scene = case scene_name of
                "rgbbox" => rgbbox
              | "irreg" => irreg
              | s => raise Fail ("No such scene: " ^ s)

val _ = print ("Using scene '" ^ scene_name ^ "' (-s to switch).\n")

val t0 = Time.now ()
val (objs, cam) = from_scene width height scene
val t1 = Time.now ()
val _ = print ("Scene BVH construction in " ^ Time.fmt 4 (Time.- (t1, t0)) ^ "s.\n")

val t0 = Time.now ()
val result = render objs width height cam
val t1 = Time.now ()

val _ = print ("Rendering in " ^ Time.fmt 4 (Time.- (t1, t0)) ^ "s.\n")

val writeImage = if dop6 then image2ppm6 else image2ppm
in
val _ = if f <> "" then
            let val out = TextIO.openOut f
            in print ("Writing image to " ^ f ^ ".\n")
               before writeImage out (render objs width height cam)
               before TextIO.closeOut out
            end
        else print ("-f not passed, so not writing image to file.\n")
end
