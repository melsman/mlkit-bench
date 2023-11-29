(* vector-sig.sml
 *
 * COPYRIGHT (c) 1993, AT&T Bell Laboratories.
 *
 * The abstract interface of vectors and matrices in some dimension.
 *)

signature REAL_VECTOR =
  sig
    type vec
    type elem = real

    val dim : int	(* dimension of the vectors *)

    val tabulate : (int -> elem) -> vec

    val equal    : vec * vec -> bool
    val zerov    : vec
    val addv     : vec * vec -> vec
    val subv     : vec * vec -> vec
    val dotvp    : vec * vec -> elem
    val crossvp  : vec * vec -> vec
    val addvs    : vec * elem -> vec
    val mulvs    : vec * elem -> vec
    val divvs    : vec * elem -> vec

    val mapv     : (elem -> elem) -> vec -> vec
    val map3v    : (elem * elem * elem -> elem) -> vec * vec * vec -> vec
    val foldv    : (elem * 'b -> 'b) -> vec -> 'b -> 'b
    val format   : {lp : string, sep : string, rp : string, cvt : elem -> string}
	           -> vec -> string
    val explode  : vec -> elem list
    val implode  : elem list -> vec

    type matrix  (* matrices are always real valued *)

    val zerom    : matrix
    val addm     : matrix * matrix -> matrix
    val outvp    : vec * vec -> matrix

    type 'a pvec
    val mappv    : (elem -> 'a) -> vec -> 'a pvec
    val foldpv   : ('a * 'b -> 'b) -> 'a pvec -> 'b -> 'b
  end
