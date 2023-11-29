(* vector3.sml
 *
 * COPYRIGHT (c) 1993, AT&T Bell Laboratories.
 *
 * 3 dimensional vector arithmetic.
 *)

structure RealVector3 : REAL_VECTOR =
  struct

    structure B = Real64Block3
    type vec = B.t
    type elem = real
    val dim = 3

    fun tabulate f = B.pack(f 0, f 1, f 2)

    val zerov = B.pack(0.0, 0.0, 0.0)

    fun equal (v1,v2) =
        Real.==(B.sub0 v1,B.sub0 v2) andalso
        Real.==(B.sub1 v1,B.sub1 v2) andalso
        Real.==(B.sub2 v1,B.sub2 v2)

    fun mapv f v =
        B.pack (f(B.sub0 v),f(B.sub1 v),f(B.sub2 v))

    fun map2v f (v1,v2) =
        B.pack (f(B.sub0 v1,B.sub0 v2),
                f(B.sub1 v1,B.sub1 v2),
                f(B.sub2 v1,B.sub2 v2))

    fun map3v f (v1,v2,v3) =
        B.pack (f(B.sub0 v1,B.sub0 v2,B.sub0 v3),
                f(B.sub1 v1,B.sub1 v2,B.sub1 v3),
                f(B.sub2 v1,B.sub2 v2,B.sub2 v3))

    val addv = map2v (op +)
    val subv = map2v (op -)

    fun foldl2 f a (v1,v2) =
        f(B.sub2 v1,B.sub2 v1,
          f(B.sub1 v1,B.sub1 v2,
            f(B.sub0 v1,B.sub0 v2,a)))

    val dotvp =	foldl2 (fn (x,y,a) => a + x*y) 0.0

    fun crossvp (v1,v2) =
        let val x1 = B.sub0 v1
            val y1 = B.sub1 v1
            val z1 = B.sub2 v1
            val x2 = B.sub0 v2
            val y2 = B.sub1 v2
            val z2 = B.sub2 v2
        in B.pack(y1*z2 - z1*y2, x1*z2 - z1*x2, x1*y2 - y1*x2)
        end

    fun addvs (v,s) = mapv (fn x => x + s) v
    fun mulvs (v,s) = mapv (fn x => x * s) v
    fun divvs (v,s) = mapv (fn x => x / s) v

    fun foldv f (v:vec) a =
        f(B.sub2 v, f(B.sub1 v, f(B.sub0 v, a)))

    fun format {lp, rp, sep, cvt} (v:vec) =
        String.concat[
	  lp, cvt (B.sub0 v),
          sep, cvt (B.sub1 v),
          sep, cvt (B.sub2 v),
          rp]

    fun explode (v:vec) = [B.sub0 v,B.sub1 v,B.sub2 v]

    fun implode [e1,e2,e3] : vec = B.pack (e1,e2,e3)
      | implode _ = raise Fail "implode: bad dimension"

    structure M = Real64Block9
    type matrix = M.t

    val zerom : matrix = M.pack(0.0, 0.0, 0.0,
                                0.0, 0.0, 0.0,
                                0.0, 0.0, 0.0)

    fun map2 f (a : matrix, b : matrix) : matrix =
        M.pack(f (M.sub0 a,M.sub0 b),f (M.sub1 a,M.sub1 b),f (M.sub2 a,M.sub2 b),
               f (M.sub3 a,M.sub3 b),f (M.sub4 a,M.sub4 b),f (M.sub5 a,M.sub5 b),
               f (M.sub6 a,M.sub6 b),f (M.sub7 a,M.sub7 b),f (M.sub8 a,M.sub8 b))

    fun addm (a : matrix, b : matrix) : matrix =
        map2 (op +) (a,b)

    fun outvp (a:vec,b:vec) : matrix =
        M.pack(B.sub0 a * B.sub0 b, B.sub0 a * B.sub1 b, B.sub0 a * B.sub2 b,
               B.sub1 a * B.sub0 b, B.sub1 a * B.sub1 b, B.sub1 a * B.sub2 b,
               B.sub2 a * B.sub0 b, B.sub2 a * B.sub1 b, B.sub2 a * B.sub2 b)

    type 'a pvec = 'a * 'a * 'a
    fun mappv f v : 'a pvec = (f (B.sub0 v), f(B.sub1 v), f(B.sub2 v))

    fun foldpv (f : 'a * 'b -> 'b) (v0,v1,v2) (a:'b) : 'b =
        f(v2,f(v1,f(v0,a)))

  end (* VectMath *)
