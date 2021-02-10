(* Second-order array combinators on pull-arrays. Many of the
   combinators are parameterised over a __granularity control
   specifier__ gcs, which is a pair (P,G) of two integers, where P
   specifies the maximum number of new threads to be spawned by the
   operator and where G specifies the minimum amount of sequential
   work each thread should perform.

   The library does not attempt at any form of dynamic
   load-ballancing. Also it does not attempt at doing anything clever
   for nested use of the combinators.

   The library uses a naming convention that allow for MLKit to
   partially inline the combinators, which allows allocation-efficient
   parallel implementations of combinators.
 *)

signature SOAC = sig
  type 'a arr            (* pull-array *)

  type gcs = int * int (* max parallelism (P), min work (Grain) *)
  val gcs_par          : gcs
  val gcs_seq          : gcs
  val gcs_split        : gcs -> gcs * gcs   (* maintain grain, put split parallelism *)

  val empty            : unit -> 'a arr
  val size             : 'a arr -> int
  val map              : ('a -> 'b) -> 'a arr -> 'b arr
  val repl             : int -> 'a -> 'a arr
  val iota             : int -> int arr
  val take             : int -> 'a arr -> 'a arr
  val drop             : int -> 'a arr -> 'a arr
  val split            : 'a arr -> 'a arr * 'a arr

  val reduce           : gcs -> ('a * 'a -> 'a) -> 'a -> 'a arr -> 'a
  val reduce__inline   : gcs -> ('a * 'a -> 'a) -> 'a -> 'a arr -> 'a

  val scan             : gcs -> ('a * 'a -> 'a) -> 'a -> 'a arr -> 'a array
  val scan__inline     : gcs -> ('a * 'a -> 'a) -> 'a -> 'a arr -> 'a array

  val toArray          : gcs -> 'a -> 'a arr -> 'a array
(*  val toArray__inline  : gcs -> 'a -> 'a arr -> 'a array *)
  val memoize          : gcs -> 'a -> 'a arr -> 'a arr
  val fromArray        : 'a array -> 'a arr

  (* combinators for nested parallelism *)
  val ppar             : gcs -> (gcs -> 'a) * (gcs -> 'b) -> 'a * 'b

  structure Array :
    sig
      val map             : gcs -> ('a -> 'b) -> 'a array -> 'b array
      val empty           : unit -> 'a array
      val filter          : gcs -> bool array -> 'a array -> 'a array
      val filter'         : gcs -> ('a -> bool) -> 'a array -> 'a array
      val filter'__inline : gcs -> ('a -> bool) -> 'a array -> 'a array
    end

  (* functions that need be exported for x-module inlining *)
  val scan__noinline   : ('a arr * int * 'a array -> unit)
                         -> ('a array * int * 'a array * int * int -> unit)
                         -> gcs -> 'a -> 'a arr -> 'a array
  val reduce__noinline : ('a arr -> 'a) -> gcs -> 'a -> 'a arr -> 'a

(*
  val toArray__noinline : ('a array -> int*int -> unit) -> 'a -> gcs -> int*int -> 'a array

  val for : int * int -> (int -> unit) -> unit
*)
end


structure SOAC: SOAC = struct

(* some utilities *)
infix |>
fun v |> f = f v

(*
fun for (lo,hi) (f:int->unit) : unit =
    let fun loop i =
            if i >= hi then ()
            else (f i; loop (i+1))
    in loop lo
    end
*)

structure A = Array

fun upd a i x = A.update (a, i, x)
fun nth a i   = A.sub (a, i)
fun empty_array () : 'a array =
    A.tabulate(0, fn _ => raise Fail "empty_array")

val parfor = ForkJoin.parfor
val parfor' = ForkJoin.parfor'
val par = ForkJoin.par
val allocate = ForkJoin.alloc

(* lo,hi,f: [f(lo),...,f(hi-1)] *)
type 'a arr = int * int * (int -> 'a)

fun fromArray (a: 'a array) : 'a arr =
    (0,Array.length a,nth a)

type gcs = ForkJoin.gcs
val maxNat : int = case Int.maxInt of SOME n => n
                                    | NONE => 1000000

val gcs_par = (maxNat,1)       (* 1: minimum sequential work *)
val gcs_seq = (0,maxNat)       (* 0: no more new threads *)

fun gcs_split (P,G) = let val P' = P div 2
                      in ((P',G),(P-P',G))
                      end

fun iota n : int arr = (0,n,fn x => x)

fun empty () : 'a arr = (0,0,fn _ => raise Fail "empty")

fun size (lo,hi,_) : int = hi - lo

fun map f (lo,hi,g) = (lo,hi,f o g)

fun repl n v = (0,n,fn _ => v)

fun take k (lo,hi,f) =
    if k >= 0 then (lo,Int.min(hi,lo+k),f)
    else (lo,hi,f)

fun drop k (lo,hi,f) =
    if k <= 0 then (lo,hi,f)
    else if k+lo >= hi then (0,0,f)
    else (lo+k,hi,f)

fun split ((lo,hi,f): 'a arr) : 'a arr * 'a arr =
    let val m = (hi-lo) div 2
    in ((lo,lo+m,f),(lo+m,hi,f))
    end

fun get ((lo,hi,f) : 'a arr) : ('a * 'a arr) option =
    if hi-lo <= 0 then NONE
    else SOME(f lo, (lo+1,hi,f))

fun foldl g a (lo,hi,f) =
    let fun loop (i,a) = if i >= hi then a
                         else loop (i+1,g(a,f i))
    in loop (lo,a)
    end

fun ppar (gcs:gcs) (f:gcs->'a,g:gcs->'b) : 'a * 'b =
    let val (gcs1,gcs2) = gcs_split gcs
    in par (fn () => f gcs1, fn () => g gcs2)
    end

fun sequential ((P,G): gcs) (n:int) : bool =
    P <= 0 orelse n <= G

fun toArray gcs (b:'a) (arr:'a arr) : 'a array =
    let val n = size arr
    in if n = 0 then empty_array()
       else let val (lo,_,f) = arr
                val result = allocate n b
            in parfor' gcs (0, n) (fn i => upd result i (f (lo+i)))
             ; result
            end
    end
(*
fun toArray__noinline doit (b:'a) gcs (lo,hi) : 'a array =
    let val n = hi-lo
    in if n <= 0 then empty_array()
       else let val result = allocate n b
            in ForkJoin.parfor'__noinline (doit result) gcs (0,n)
             ; result
            end
    end

fun toArray__inline gcs (b:'a) ((lo,hi,f):'a arr) : 'a array =
    let fun doit (result:'a array) (a,b) : unit =
            for (a,b) (fn i => upd result i (f (lo+i)))
    in toArray__noinline doit b gcs (lo,hi)
    end

val toArray = toArray__inline
*)
fun memoize (gcs:gcs) (b:'a) (arr:'a arr) : 'a arr =
    toArray gcs b arr |> fromArray

(* geee - when g is passed, the argument pair to g is forced to be in
   a region which is fixed for (and global to) all calls to g within
   scan... We solve this issue by partial inlining... *)

datatype segm = SEQ | PAR of {blks:int, blksz:int}

fun parallel ((P,G):gcs) (n:int) : segm =
    if P <= 0 orelse n <= G then SEQ
    else let val blks = Int.min(P+1,1 + (n-1) div G) (* number of blocks *)
             val blksz = n div blks
             val blksz = if n > blksz*blks then blksz+1 else blksz
         in PAR {blks=blks, blksz=blksz}
         end

fun scan__noinline (seqScan: 'a arr * int * 'a array -> unit)
                    (seqDist: 'a array * int * 'a array * int * int -> unit)
                    (gcs:gcs) (b:'a) (arr:'a arr) : 'a array =
    let val n = size arr
        val result = allocate n b
    in case parallel gcs n of
           SEQ => (seqScan (arr, 0, result); result)
         | PAR {blks,blksz} =>
           let val (P,_) = gcs
               (* Sequentially scan each local block in parallel *)
               val () = print "Computing local scans\n"
               val () = parfor' (P,1) (0,blks)
                                (fn i =>
                                    let val off = i*blksz
                                        val arr' = drop off arr |> take blksz
                                    in seqScan (arr', off, result)
                                    end)
               (* Scan of sums *)
               val () = print "Scanning sums\n"
               val sums : 'a arr =
                   (0,blks,fn i => nth result (Int.min(n-1,(i+1)*blksz-1)))
               val ssums : 'a array =
                   allocate blks b
               val () = seqScan (sums, 0, ssums)
               (* Distribute sums in each block *)
               val () = print "Distribute sums\n"
               val () = parfor' (P,1) (1,blks)
                                (fn i => seqDist (ssums, blksz, result, n, i))
           in result
           end
    end

(* inclusive scan *)
fun scan__inline (gcs:gcs) (g:'a*'a->'a) (b:'a) (arr:'a arr) : 'a array =
    let fun seqScan (arr:'a arr, tgt_offset:int, tgt:'a array) : unit =
            let val (lo,hi,f) = arr
                val n = size arr
                fun loop (b,i) =
                    if i >= n then ()
                    else let val v = f(lo+i)
                             val b = g(b,v)
                         in upd tgt (tgt_offset+i) b
                          ; loop (b,i+1)
                         end
            in loop (b,0)
            end
        fun seqDist (ssums, blksz, result, n, i) =
            let val s = nth ssums (i-1)
                fun loop (j,hi) =
                    if j >= hi then ()
                    else ( upd result j (g(s,nth result j))
                         ; loop (j+1,hi))
            in loop (i*blksz,Int.min(n,(i+1)*blksz))
            end
    in scan__noinline seqScan seqDist gcs b arr
    end

val scan = scan__inline

fun reduce__noinline (seqRed: 'a arr -> 'a) (gcs:gcs) (b:'a) (arr:'a arr) : 'a =
    case parallel gcs (size arr) of
        SEQ => seqRed arr
      | PAR {blks,blksz} =>
        let val (P,_) = gcs
            val sums = allocate blks b
            val () = parfor' (P,1) (0,blks)
                             (fn i => drop (i*blksz) arr |>
                                           take blksz |>
                                           seqRed |>
                                           upd sums i)
        in seqRed (0,blks,nth sums)
        end

fun reduce__inline (gcs:gcs) (g:'a*'a->'a) (b:'a) (arr:'a arr) : 'a =
    let fun seqRed (arr:'a arr) : 'a = foldl g b arr
    in reduce__noinline seqRed gcs b arr
    end

val reduce = reduce__inline

structure Array = struct

val empty = empty_array

fun map gcs (f:'a -> 'b) (arr: 'a array) : 'b array =
    let val sz = Array.length arr
    in if sz = 0 then empty()
       else let val b = f(nth arr 0)
                val tgt = allocate sz b
            in upd tgt 0 b
             ; parfor' gcs (0,sz)
                       (fn i => upd tgt i (f(nth arr i)))
             ; tgt
            end
    end

fun filter (gcs:gcs) (bs: bool array) (xs: 'a array) : 'a array =
    let val bs_sz = Array.length bs
        val xs_sz = Array.length xs
    in if bs_sz <> xs_sz then raise Size
       else if xs_sz = 0 then xs
       else let val fs = map gcs (fn b => if b then 1 else 0) bs
                val is = scan__inline gcs (op +) 0 (fromArray fs)     (* inclusive scan *)
                val sz = nth is (bs_sz - 1)
            in if sz = 0 then empty()
               else let val b = nth xs 0
                        val result = allocate sz b
                    in parfor' gcs (0,bs_sz)
                               (fn i => if nth bs i then
                                          upd result (nth is i - 1) (nth xs i)
                                        else ()
                               )
                     ; result
                    end
            end
    end

fun filter'__inline (gcs:gcs) (f: 'a -> bool) (xs:'a array) : 'a array =
    let val bs = map gcs f xs
    in filter gcs bs xs
    end

val filter' = filter'__inline


end

end
